{-# LANGUAGE
  DefaultSignatures
, DeriveGeneric
, GeneralizedNewtypeDeriving
, LambdaCase
, MultiParamTypeClasses
, MultiWayIf
, NoOverloadedStrings
, ScopedTypeVariables
, ViewPatterns
#-}
-- | Trie: an effecient set of sequences whose elements permit an order, e.g. strings ([Char] where ∃ compare :: Char -> Char -> Ordering) or delimited strings (e.g. '/'-delimited URL paths: string/string/string/... -> Trie string where each level represents a '/', e.g. level 1's "baz" and level 2's "foo" represents "baz/foo")
-- likely you'll want to import this module qualified
-- I don't implement common classes because Trie a should really be (Traversable t, Ord a) => Trie (t a), since Trie is defined only for traversables of ordered elements, but that'd require ExistentialTypes, which would be ugly, or dependent types (which obviously Haskell doesn't support)
-- so in practice, just convert between Trie a and Set a to get good foldability &c
-- ...come to think of it, a trie is just a space-efficient version of an unordered set of ordered sets. So I should be able to use this for storing application processes.
module NicLib.Structures.Trie
( Trie
, TagPair(..)
, delete
, empty
, findMax
, findMin
, foldl'
, foldr
, forM_
, insert
, map
, mapM_
, member
, notMember
, null
, singleton
, size
, toList
, toSet
, update
{-
, filter
, difference
, (\\)
, find
, intersection
, union
, isProperSubsetOf
, isSubsetOf
, isSupersetOf
, isProperSupersetOf
, lookupGE
, lookupGT
, lookupLE
, lookupLT
, split
, fromSet
-}
) where
import Control.Arrow
import Data.Binary
import Data.Bool (bool)
import Data.ListLike (ListLike)
import Data.Maybe
import Data.Ord
import GHC.Generics (Generic)
import Prelude hiding (zip, map, null, foldr, mapM_)
import Data.List (uncons)
import qualified Data.ListLike as LL
import qualified Data.Set as S
import Data.Function ((&))
import NicLib.NStdLib (TagPair(..), cT)
import NicLib.Set (setFind)

-- | Store a set of Tries. This is a root dummy node essentially; it allows many sequences with different initial elements to be in the same data type
-- a Trie may be S.empty
newtype Trie val tag = Trie {getTrie :: S.Set (Trie' val tag)} deriving (Generic, Eq, Ord)
instance (Binary a, Binary tag) => Binary (Trie a tag)

-- | a Stop tells that the path from root to that stop is an item in the set, e.g. c -> a -> t -> {Stop (), a -> m -> a -> r -> a -> n -> Stop ()} <-> {("cat", ()), ("catamaran", ())}
-- children will never be S.null; the smallest a children can be is the set S.fromList [Stop smth]
-- you may tag a Stop element with any data you wish; the Stop data is data that belongs with the element stored in the set, but only belongs to that whole element altogether, not having any meaning in the mere context of an element of the sequence stored as an element of the trie; for example, you may want to store a trie of (String, Int), where the Int is associated with a string, but not any character.
-- remember that Stops have no effect on the Trie's organization; they're merely tagged-on for inserting and getting items. In other words, Stop values don't affect ordering of elements, trie membership, ...or anything other than "going-with" insert & singleton, and get functions (e.g. findMin, toSet)
data Trie' val tag = Trie' {val :: val, children :: Trie val tag} | Stop tag deriving (Generic,Show)
instance (Show item, Eq item, Show tag) => Show (Trie item tag) where -- TODO: this could be better, but it's plenty good enough for use as it is
    show = (\case [] -> "[EMPTY]"; s -> init s) . go 1 where
        go tab (getTrie -> trie) = flip foldMap trie $ \t -> (++)
            ({- take ((tab - 1) * 4 - 1) (foldMap (bool "─" "├" . ((== 0) . (`mod` tab))) [0..]) -} (replicate ((tab - 1) * 4 - 1) ' ') ++ ((if (case t of Stop _ -> S.size trie == 1; Trie' v _ -> v == val (S.findMax trie)) then '└' else '├'):"─"))
            (' ':(case t of Stop v -> "[TAG] " ++ show v ++ "\n"; Trie' v k -> show v ++ "\n" ++ go (succ tab) k))
instance (Binary a, Binary tag) => Binary (Trie' a tag)

-- | helper method. returns (Stop, non-Stops); if no Stop, returns Nothing (in which case the right side of the partition pair is equal to the Trie passed to it)
factorStop :: Trie val tag -> Maybe (tag, Trie val tag)
factorStop = (\(s,r) -> if S.null s then Nothing else Just (case S.elemAt 0 s of Stop sv -> sv, Trie r)) . S.partition (\case Stop _ -> True; _ -> False) . getTrie

-- | helper method. Whether a Trie contains a Stop element
hasStop :: Trie a b -> Bool
hasStop = isJust . factorStop

empty :: (Ord a) => Trie a b
empty = Trie S.empty

null :: (Ord a) => Trie a b -> Bool
null = S.null . getTrie

singleton :: (ListLike full item, Ord item) => (full, tag) -> Trie item tag
singleton (full, tag) = Trie $ LL.foldr (\z c -> S.singleton (Trie' z (Trie c))) (S.singleton (Stop tag)) full

delete :: forall full item tag. (ListLike full item, Ord item) => full -> Trie item tag -> Trie item tag
delete ss tt =
    if LL.length ss /= length h then tt else
        flip (maybe tt) t $ \(snd -> t_nonStops) ->
            if null t_nonStops then -- there do not exist any items in trie longer than the one we're deleting (e.g. "catamaran" from trie {"cat", "car", "catamaran"})
                case dropWhile (\n -> maybe (null n) (const False) (factorStop n)) h of -- delete parents until the first with a non-null non-stop set
                    [] -> zip (LL.empty :: full) empty [last h]
                    (fh:rh) -> zip (LL.take (length rh) ss) fh rh
            else -- there are items longer than the one we're deleting (e.g. we're deleting "cat" from trie {"cat", "car", "catamaran"}
                zip ss t_nonStops h -- so just delete the Stop at the matched node, and zip it up
    where
        (factorStop -> t, h) = match ss tt

member :: (ListLike full item, Ord item) => full -> Trie item tag -> Bool
member ss = (\(t,h) -> hasStop t && LL.length ss == length h) . match ss

notMember :: (ListLike full item, Ord item) => full -> Trie item tag -> Bool
notMember = cT not member

-- | insert item into trie if item is not already there; if item is in trie already (independent of its tag value) then trie is unaltered. Use method update to update an already existant item's tag value 
insert :: (ListLike full item, Ord item) => (full, tag) -> Trie item tag -> Trie item tag
insert (ss, tag) t0 = case match ss t0 of 
    (t,h) -> if hasStop t && LL.length ss == length h then t0 else
        case LL.splitAt (length h) ss of (p1, p2) -> zip p1 (Trie $ S.union (getTrie $ singleton (p2, tag)) (getTrie t)) h

-- | inserts item into trie. If item is already in trie, updates its tag value
update :: (ListLike full item, Ord item) => (full, tag) -> Trie item tag -> Trie item tag
update (ss, tag) = match ss >>> \(t,h) -> case LL.splitAt (length h) ss of (p1, p2) -> zip p1 (Trie $ S.union (getTrie $ singleton (p2, tag)) (getTrie t)) h

{- functions that'd make this module complete, but that I won't be using in the forseeable future
change all [a] to ListLike full a.
-- | filters branches
filter :: Ord a => ([a] -> Bool) -> Trie a -> Trie a
filter = undefined

difference :: Ord a => Trie a -> Trie a -> Trie a
difference = undefined

infixl 6 \\
(\\) :: Ord a => Trie a -> Trie a -> Trie a
(\\) = difference

intersection :: Ord a => Trie a -> Trie a -> Trie a
union :: Ord a => Trie a -> Trie a -> Trie a
isProperSubsetOf :: Ord a => Trie a -> Trie a -> Bool
isSubsetOf :: Ord a => Trie a -> Trie a -> Bool
isSupersetOf :: Ord a => Trie a -> Trie a -> Bool
lookupGE :: Ord a => [a] -> Trie a -> Maybe a
lookupGT :: Ord a => [a] -> Trie a -> Maybe a
lookupLE :: Ord a => [a] -> Trie a -> Maybe a
lookupLT :: Ord a => [a] -> Trie a -> Maybe a

split :: Ord a => Trie a -> Trie a -> (Trie a, Trie a)
-}

foldr :: (ListLike full a, Ord a) => ((full, tag) -> b -> b) -> b -> Trie a tag -> b
foldr f = go LL.empty where -- traverse down trie, building accumulator until a Stop is hit, then apply function to that branch. Base case for go is t = fromList [Stop], in which case go returns lump: (\x -> S.foldr undefined x S.empty) = id
    go acc lump t = case (\(Trie' v c) b -> go (LL.snoc acc v) b c) of
        ff -> case factorStop t of
           Nothing -> S.foldr ff lump (getTrie t)
           Just (stopVal, nonStops) -> S.foldr ff (f (acc, stopVal) lump) (getTrie nonStops)

foldl' :: (ListLike full a, Ord a) => (b -> (full, tag) -> b) -> b -> Trie a tag -> b
foldl' f = go LL.empty where -- traverse down trie, building accumulator until a Stop is hit, then apply function to that branch. Base case for go is t = fromList [Stop], in which case go returns lump: (\x -> S.foldr undefined x S.empty) = id
    go acc lump t = case (\b (Trie' v c) -> go (LL.snoc acc v) b c) of
        ff -> case factorStop t of
           Nothing -> S.foldl' ff lump (getTrie t)
           Just (stopVal, nonStops) -> S.foldl' ff (f lump (acc, stopVal)) (getTrie nonStops)

-- | apply a function to each element of a trie, in order of its elements (e.g. mapM_ print {("ao",()), ("hello",()), ("hi",())} will print ao, then hello, then hi)
mapM_ :: (ListLike full item, Ord item, Monad m) => ((full, tag) -> m ()) -> Trie item tag -> m ()
mapM_ f = foldr (\x y -> f x >> y) (return ())

forM_ :: (ListLike full item, Ord item, Monad m) => Trie item tag -> ((full, tag) -> m ()) -> m ()
forM_ = flip mapM_

-- | number of elements in the set
size :: Ord a => Trie a tag -> Int
size = foldr (const succ :: ([a], tag) -> Int -> Int) 0 -- arbitrary ListLike instance [a] a chosen to satisfy typechecker

-- | first mapping function must preserve order! An example map would be (,()) 
map :: (Ord a, Ord b) => (a -> b) -> (c -> d) -> Trie a c -> Trie b d
map f g = Trie . S.map (\case Trie' v k -> Trie' (f v) (map f g k); Stop stopVal -> Stop (g stopVal)) . getTrie

findMax :: (ListLike full item) => Trie item tag -> (full, tag)
findMax = mmCommon S.findMax

findMin :: (ListLike full item) => Trie item tag -> (full, tag)
findMin = mmCommon S.findMin

-- | helper function used in findMin/Max
mmCommon :: (ListLike full item) => (S.Set (Trie' item tag) -> Trie' item tag) -> Trie item tag -> (full, tag)
mmCommon f = go LL.empty where
    go h t = case f (getTrie t) of
        Stop stopVal -> (LL.reverse h, stopVal)
        Trie' v k -> go (LL.cons v h) k

-- | you'll probably need to specify the type of ListLike, e.g. (toSet trie :: S.Set String)
toSet :: (ListLike full a, Ord a, Ord full) => Trie a tag -> S.Set (TagPair full tag)
toSet = foldr (S.insert . TagPair) S.empty

toList :: (ListLike full a, Ord a, Ord full, Ord tag) => Trie a tag -> [(full, tag)]
toList = foldr (:) []

instance Eq a => Eq (Trie' a tag) where
    Trie' a s == Trie' b t = a == b && s == t
    Stop _ == Stop _ = True
    Stop _ == Trie' _ _ = False
    Trie' _ _ == Stop _ = False

-- | Tries are ordered by the first element by which they differ
instance Ord a => Ord (Trie' a tag) where
    compare (Trie' a s) (Trie' b t) = case compare a b of EQ -> compare s t; o -> o
    compare (Trie' _ _) (Stop _) = GT -- Stop is always minimum
    compare (Stop _) (Trie' _ _) = LT
    compare (Stop _) (Stop _) = EQ

-- Match & Zip: Effectively Duals
-- (\s t -> (\(x,y) -> zip s x y) (match s t)) z a == a, where z is any string and a is any Trie

-- | helper function. Searches through a trie for branch matching longest substring of given sequence. Returns (matched trie t, stack of tries ancestors of t \ t)
match :: (ListLike full item, Ord item) => full -> Trie item tag -> (Trie item tag, [Trie item tag])
match ss t = go ss t [] where
    go :: (ListLike full item, Ord item) => full -> Trie item tag -> [Trie item tag] -> (Trie item tag, [Trie item tag])
    go s t'@(getTrie -> t) h =
        do
            (c,cs) <- LL.uncons s
            x <- setFind ((c==) . val) (maybe t (getTrie . snd) (factorStop $ Trie t))
            return (x,cs)
        & \case
            Nothing -> (t', h) -- return what we have
            Just (x,cs) -> go cs (children x) (Trie (S.delete x t):h) -- create Trie' with c

-- | inserts a sub-Trie' into a larger Trie, using a stack whose head is childmost Trie
-- first parameter gives the values of Trie's for childless nodes in the history stack
zip :: (ListLike full item, Ord item) => full -> Trie item tag -> [Trie item tag] -> Trie item tag
zip (LL.reverse -> s) = cT (fst . flip (LL.foldl' (\i@(t, (h:hs)) c -> (Trie $ S.insert (Trie' c t) (getTrie h), hs))) s) (,) -- s is guaranteed to be at least as long as h, but in practice should always be the same length. Thus zip should throw any index out of bounds error, as it indicates a logic error in whatever method uses zip
