-- TODO: specialize, and rewrite in much more maintainable fashon, and test throughly, especially after recent refactoring into mono-traversable!
-- | A space-effecient set of sequences whose elements permit an order; isomorphic with @Ord a => Set [a]@. Uses 'Set' and @mono-traversable@ under-the-hood.
--
-- Strict in keys but not values.
--
-- Likely you'll want to import this module qualified.
module NicLib.Structures.Trie
( Trie
, delete
, empty
, findMax
, findMin
, foldl'
, foldr
, insert
, bimap
, traverse_
, member
, lookup
, diff
, notMember
, null
, singleton
, size
, toList
, fromFoldable
, toSet
, update
, filter
, difference
, (\\)
, union
, isProperSubsetOf
, isSubsetOf
{-
, lookupGE
, lookupGT
, lookupLE
, lookupLT
-}
) where

-- rio & deps
import RIO hiding (foldl', reverse, foldr, zip, lookup, null, replicate, dropWhile, take, traverse_, toList, filter)
import Data.Bool (bool)
import Data.Function (fix)
import qualified Data.Foldable as Fld
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (init)

-- containers
import Data.Sequence (Seq)

-- NicLib
import NicLib.NStdLib (OrderBy(..), cT, (<%))
import NicLib.Set (setFind)

-- mono-traversable
import Data.Sequences hiding (delete, singleton, filter)
import Data.MonoTraversable

newtype Trie val tag = Trie { getTrie :: Set (Trie' val tag) } deriving (Eq, Ord)

--  A Stop tells that the path from root to that stop is an element in the set, e.g. c -> a -> t -> {Stop (), a -> m -> a -> r -> a -> n -> Stop ()} <-> {("cat", ()), ("catamaran", ())}
-- children will never be @S.null@; the smallest a children can be is the set @S.fromList [Stop _]@. Also, @Stop@s are always the minimum element of any trie.
-- you may tag a Stop element with any data you wish; the Stop data is data that belongs with the element stored in the set, but only belongs to that whole element altogether, not having any meaning in the mere context of an element of the sequence stored as an element of the trie; for example, you may want to store a trie of (String, Int), where the Int is associated with a string, but not any character.
-- remember that Stops' values have no effect on the Trie's organization; they're merely tagged-on for inserting and getting elements. In other words, Stop values don't affect ordering of elements, trie membership, ...or anything other than "going-with" insert & singleton, and get functions (e.g. findMin, toSet)
data Trie' val tag = Trie' { val :: val, children :: !(Trie val tag) } | Stop tag deriving Show
instance (Show e, Ord e, Show tag) => Show (Trie e tag) where
    show t0 | null t0   = "[EMPTY]"
            | otherwise = init (go 1 S.empty t0)
        where
            indentAmt = 4 -- number of spaces that makes an indentation
            go !tab !vbars (getTrie -> !trie) = flip foldMap trie $! \t -> -- go is recursive
                let isMaxElement = case t of
                        Stop _ -> S.size trie == 1
                        Trie' !v _ -> v == val (S.findMax trie)
                    graphicBranch = fix (\f !s !i -> if i < tab then f (s ++ bool " " "│" (i `S.member` vbars) ++ replicate (indentAmt - 1) ' ') (i + 1) else s) "" 1 ++ bool "├" "└" isMaxElement
                in graphicBranch ++ '─':' ':(case t of Stop !v -> "[TAG] " ++ show v ++ "\n"; Trie' !v !k -> show v ++ "\n" ++ go (tab + 1) (if isMaxElement then vbars else S.insert tab vbars) k)

-- | Add elements from one trie into another. Elements present in both tries have @<>@ applied to their tags.
instance (Ord a, Semigroup tag) => Semigroup (Trie a tag) where
    (<>) !a !b = foldl' (\ !t !seq tag -> update' seq tag t) a b where
        -- update modified for (<>) rather than replacement
        update' :: Ord seq => [seq] -> tag -> Trie seq tag -> Trie seq tag
        update' !ss tag = match ss >>> \(!t,!h) -> case splitAt (length h) ss of
            (!p1, !p2) ->
                let !y = if olength ss == length h then fromMaybe tag $ (<> tag) . fst <$> factorStop t else tag -- this is a modified lookup function
                    !x = Trie $! S.union (getTrie $ singleton p2 y) (getTrie t)
                    -- NB. update = update' when y = tag
                in zip p1 x h

instance (Monoid tag, Ord a) => Monoid (Trie a tag) where
    mempty = empty -- this is Trie.empty, not Applicative.empty!
    mappend = (<>)

-- | First map (the one over values, not tags) must be monotonic! Be careful, as this condition cannot be checked, and behavior is undefined for non-monotonic mappings!
--
-- Not an instance of @Bifunctor@ because this @bimap@ requires that the elements being mapped over instance @Ord@.
bimap :: (Ord a, Ord b) => (a -> b) -> (c -> d) -> Trie a c -> Trie b d
bimap f g = Trie . S.map (\case Trie' !v !k -> Trie' (f v) (bimap f g k); Stop stopVal -> Stop (g stopVal)) . getTrie

-- helper method. returns (Stop, non-Stops); if no Stop, returns Nothing (in which case the right side of the partition pair is equal to the Trie passed to it)
-- inexhaustive pattern match against Stop only is OK
factorStop :: Trie val tag -> Maybe (tag, Trie val tag)
factorStop = (\(s,!r) -> if S.null s then Nothing else Just (case S.elemAt 0 s of Stop sv -> sv, Trie r)) . S.partition (\case Stop _ -> True; _ -> False) . getTrie

-- | helper method. Whether a Trie contains a Stop element
hasStop :: Trie a b -> Bool
hasStop = isJust . factorStop

empty :: (Ord a) => Trie a b
empty = Trie S.empty

null :: (Ord a) => Trie a b -> Bool
null = S.null . getTrie

singleton :: Foldable t => t val -> tag -> Trie val tag
singleton !seq tag = Trie $ Fld.foldr -- this must be foldr; using foldl' will put the trie in reverse order!
    (\z c -> S.singleton . Trie' z $ Trie c)
    (S.singleton $ Stop tag)
    seq

delete :: forall seq e tag. (IsSequence seq, Index seq ~ Int, Monoid seq, e ~ Element seq, Ord e) => seq -> Trie e tag -> Trie e tag
delete !ss !tt =
    if olength ss /= length h then tt else
        flip (maybe tt) t $ \(snd -> !t_nonStops) ->
            if null t_nonStops then -- there do not exist any elements in trie longer than the one we're deleting (e.g. "catamaran" from trie {"cat", "car", "catamaran"})
                case dropWhile (\n -> maybe (null n) (const False) (factorStop n)) h of -- delete parents until the first with a non-null non-stop set
                    [] -> zip (mempty :: seq) empty [lastEx h]
                    (fh:rh) -> zip (take (lengthIndex rh) ss) fh rh
            else -- there are elements longer than the one we're deleting (e.g. we're deleting "cat" from trie {"cat", "car", "catamaran"}
                zip ss t_nonStops h -- so just delete the Stop at the matched node, and zip it up
    where
        (factorStop -> t, h) = match ss tt

member :: (IsSequence seq, e ~ Element seq, Ord e) => seq -> Trie e tag -> Bool
member ss = match ss >>> \(t,h) -> hasStop t && olength ss == length h

notMember :: (IsSequence seq, e ~ Element seq, Ord e) => seq -> Trie e tag -> Bool
notMember = cT not member

-- | Get an index's associated tag
lookup :: (IsSequence seq, e ~ Element seq, Ord e) => seq -> Trie e tag -> Maybe tag
lookup !ss = match ss >>> \(t,h) -> if olength ss == length h then fst <$> factorStop t else Nothing

-- | Insert element into trie if it is not already there; if it is in trie already (independent of its tag value) then trie is unaltered. Use method update to update an already existant element's tag value 
insert :: (Foldable t, seq ~ t e, e ~ Element seq, IsSequence seq, Ord e, Index seq ~ Int) => seq -> tag -> Trie e tag -> Trie e tag
insert ss tag t0 = case match ss t0 of 
    (t,h) -> let lh = length h in if hasStop t && olength ss == lh then t0 else
        case splitAt lh ss of (p1, p2) -> zip p1 (Trie $ S.union (getTrie $ singleton p2 tag) (getTrie t)) h

-- | Inserts element into trie. If it's already in trie, updates its tag value
update :: (Foldable t, seq ~ t e, e ~ Element seq, IsSequence seq, Ord e, Index seq ~ Int) => seq -> tag -> Trie e tag -> Trie e tag
update ss tag = match ss >>> \(t,h) -> case splitAt (length h) ss of (p1, p2) -> zip p1 (Trie $ S.union (getTrie $ singleton p2 tag) (getTrie t)) h

-- | Filters branches
filter :: (Foldable t, seq ~ t e, e ~ Element seq, IsSequence seq, Ord e, Index seq ~ Int) => (seq -> tag -> Bool) -> Trie e tag -> Trie e tag
filter p = foldr (\f t b -> if p f t then insert f t b else b) empty

difference :: (Ord a, t ~ Trie a tag) => t -> t -> t
difference (Trie a) (Trie b) = Trie $ a S.\\ b

-- infixl 6 \\ -- for SOME reason, though this works fine here, at lesat one project which has NicLib as a dependency has a parse error at fixity definition of this
(\\) :: (Ord a, t ~ Trie a tag) => t -> t -> t
(\\) = difference

union :: (Ord a, t ~ Trie a tag) => t -> t -> t
union (Trie a) (Trie b) = Trie $ a `S.union` b

isProperSubsetOf :: (Ord a, t ~ Trie a tag) => t -> t -> Bool
isProperSubsetOf (Trie a) (Trie b) = a `S.isProperSubsetOf` b

isSubsetOf :: (Ord a, t ~ Trie a tag) => t -> t -> Bool
isSubsetOf (Trie a) (Trie b) = a `S.isSubsetOf` b

{- I don't care to implement these boilerplates yet.
lookupGE :: Ord a => [a] -> Trie a -> Maybe a
lookupGT :: Ord a => [a] -> Trie a -> Maybe a
lookupLE :: Ord a => [a] -> Trie a -> Maybe a
lookupLT :: Ord a => [a] -> Trie a -> Maybe a
-}

-- | (A \\ B, B \\ A, A ∩ B) of two tries A and B. The binary "merge" function is performed on elements that are present in both tries (since we can't have duplicate elements in the result trie.) Some common examples are @curry fst@, @curry snd@, or @(,)@.
diff :: forall a tag tag2. (Ord a) => Trie a tag -> Trie a tag -> (tag -> tag -> tag2) -> (Trie a tag, Trie a tag, Trie a tag2)
diff a b m = foldr f (empty, b, empty) a
    where
        f :: Seq a -> tag -> (Trie a tag, Trie a tag, Trie a tag2) -> (Trie a tag, Trie a tag, Trie a tag2) -- I just chose Seq to satisfy ListLike. One day I should really profile various LL instances!
        f x xt (ja, jb, ovrlp) = case lookup x b of
            Nothing -> (insert x xt ja, jb, ovrlp)
            Just t -> (ja, delete x jb, insert x (m xt t) ovrlp)

-- | Can't instance @Bifoldable@ because @foldr@'s context is more specific than @bifoldr@'s (namely the @Ord@ and @ListLike@-ness)
foldr :: (SemiSequence seq, Monoid seq) => (seq -> tag -> b -> b) -> b -> Trie (Element seq) tag -> b
foldr f = go mempty where -- traverse down trie, building accumulator until a Stop is hit, then apply function to that branch. Base case for go is t = fromList [Stop], in which case go returns lump: (\x -> S.foldr undefined x S.empty) = id
    go acc lump t = case (\(Trie' v c) b -> go (snoc acc v) b c) of
        ff -> case factorStop t of
           Nothing -> S.foldr ff lump (getTrie t)
           Just (stopVal, Trie nonStops) -> S.foldr ff (f acc stopVal lump) nonStops

foldl' :: (SemiSequence seq, Monoid seq) => (b -> seq -> tag -> b) -> b -> Trie (Element seq) tag -> b
foldl' f = go mempty where
    go acc lump t = case (\b (Trie' v c) -> go (snoc acc v) b c) of
        ff -> case factorStop t of
           Nothing -> S.foldl' ff lump (getTrie t)
           Just (stopVal, Trie nonStops) -> S.foldl' ff (f lump acc stopVal) nonStops

-- | Apply a function to each element of a trie, in order of its elements, e.g.
--
-- >>> traverse_ (\x _ -> print x) {("ao",()), ("hello",()), ("hi",())}
-- prints ao, then hello, then hi
traverse_ :: (SemiSequence seq, Monoid seq, Applicative f) => (seq -> tag -> f ()) -> Trie (Element seq) tag -> f ()
traverse_ f = foldr (\x y z -> f x y *> z) (pure ())

-- | Number of elements in the set
size :: forall a i tag. (Ord a, Integral i) => Trie a tag -> i
size = foldr ((\_ _ -> (+1)) :: [a] -> tag -> i -> i) 0 -- arbitrary ListLike instance [a] a chosen to satisfy typechecker

findMax :: (SemiSequence seq, Monoid seq) => Trie (Element seq) tag -> (seq, tag)
findMax = mmCommon S.findMax

findMin :: (SemiSequence seq, Monoid seq) => Trie (Element seq) tag -> (seq, tag)
findMin = mmCommon S.findMin

-- | helper function used in findMin/Max
mmCommon :: (SemiSequence seq, Monoid seq, e ~ Element seq) => (Set (Trie' e tag) -> Trie' e tag) -> Trie e tag -> (seq, tag)
mmCommon f = go mempty where
    go h t = case f (getTrie t) of
        Stop stopVal -> (reverse h, stopVal)
        Trie' v k -> go (cons v h) k

-- | You'll probably need to specify the type of ListLike, e.g. (toSet trie :: Set String)
toSet :: (SemiSequence seq, Monoid seq, Ord seq) => Trie (Element seq) tag -> Set (OrderBy seq tag)
toSet = foldr (S.insert <% OrderBy) S.empty

toList :: (SemiSequence seq, Monoid seq) => Trie (Element seq) tag -> [(seq, tag)]
toList = foldr ((:) <% (,)) []

fromFoldable :: (IsSequence seq, e ~ Element seq, Ord e, Ord seq, Foldable t, seq ~ t e, Index (t e) ~ Int) => t (seq, tag) -> Trie e tag
fromFoldable = Fld.foldr (\(f, t) b -> update f t b) empty

instance Eq a => Eq (Trie' a tag) where
    Trie' a s == Trie' b t = a == b && s == t
    Stop _ == Stop _ = True
    Stop _ == Trie' _ _ = False
    Trie' _ _ == Stop _ = False

-- | Tries are ordered by the first element by which they differ
instance Ord a => Ord (Trie' a tag) where
    compare (Trie' a s) (Trie' b t) = case compare a b of EQ -> compare s t; o -> o
    compare (Trie' _ _) (Stop _) = GT
    compare (Stop _) (Trie' _ _) = LT
    compare (Stop _) (Stop _) = EQ

-- Match & Zip: Effectively Duals
-- (\s t -> (\(x,y) -> zip s x y) (match s t)) z a == a, where z is any string and a is any Trie

-- TODO: match is (at least one of) the most expensive and elementary functions in Trie. Many methods depend on its output ((t,h) -> ...); make this combinatory.
-- helper function. Searches through a trie for branch matching longest substring of given sequence. Returns (matched trie t, stack of tries ancestors of t \ t)
match :: forall seq e t tag. (IsSequence seq, e ~ Element seq, Ord e, t ~ Trie e tag) => seq -> t -> (t, [t])
match ss t = go ss t [] where
    go :: seq -> t -> [t] -> (t, [t])
    go !s !t'@(getTrie -> t) !h = maybe (t', h) (\(!x,!cs) -> go cs (children x) (Trie (S.delete x t):h)) $ do
        (c,cs) <- uncons s
        x <- setFind ((c==) . val) (maybe t (getTrie . snd) (factorStop t'))
        pure (x,cs)

-- inserts a sub-Trie' into a larger Trie, using a stack whose head is childmost Trie
-- first parameter gives the values of Trie's for childless nodes in the history stack
zip :: (IsSequence seq, e ~ Element seq, Ord e, t ~ Trie e tag) => seq -> t -> [t] -> t
zip (reverse -> s) = fst . flip (ofoldl' (\(t, h:hs) c -> (Trie $ S.insert (Trie' c t) (getTrie h), hs))) s <% (,) -- s is guaranteed to be at least as long as h, but in practice should always be the same length. Thus zip should throw an index out of bounds error, as it indicates a logic error in whatever method uses zip
