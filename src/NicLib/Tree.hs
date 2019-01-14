-- | This module was written for HTML manipulation. Since discovering the hxt package, all of the predicate-based tree filters herein are deprecated and thusly unmaintained. They will likely be removed in future versions. In fact, I'm doubtful that one should concern themselves with trees not expressed as graphs or zippers. I've yet to study the practicality of using (or especially manipulating) non-graph non-zipper trees, enough to determine to what extent such trees are useful. I'm also very keen on learning how hxt uses arrows; upon which structures do arrows operate, and how efficient are these operations?
--
-- 'readIndentedGeneral' and friends are still relevant, however.
module NicLib.Tree
( Rel(..)
  -- * Filters
, findByPath
, matchTree
, selectBranches
, pruneTree
-- * Utilities
, putInTree
, toGraph
, toGraph'
-- * Morphisms from Tree
, toPaths
, showIndented
-- * Morphisms to Tree
-- , fromPaths
, readIndentedGeneral
, readIndentedText
, readIndentedTextLazy
) where

-- base
import Control.Applicative (empty)
import Control.Arrow ((***), second)
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Foldable
import Data.List (uncons)
import Data.Maybe (maybeToList)
import Data.Semigroup (mtimesDefault)
import Data.List.NonEmpty (NonEmpty(..)) -- v4.9.0.0+
import qualified Data.List.NonEmpty as NE

-- containers
import Data.Sequence (Seq)
import Data.Tree
import qualified Data.Set as S
import qualified Data.Map.Lazy as M

-- NicLib
import NicLib.AccumShort (liftME)
import NicLib.NStdLib (bool')

-- misc.
import qualified Data.Bifunctor as BiF -- bifunctors
import qualified Data.ListLike as LL -- ListLike

-- text
import qualified Data.Text as T'
import qualified Data.Text.Lazy as T

-- fgl
import Data.Graph.Inductive.Graph hiding (empty)

data Rel = Child | Sibling | Parent deriving (Show, Eq, Ord)

-- | Returns branches matching the ordered list of predicates.
--
-- Returns Nothing if root fails to match. Note that the maximum depth of the returned tree is the length of the predicate list; to take all children past a given depth, use repeat, e.g. to get a Tree consisting of all branches from \<!doctype\> to descendants of \<head\>
--
-- @
-- findByPath ((getTag \<$\> [\"DOCTYPE\", "html", "head"]) ++ repeat (const True)) tree
--     where
--         getTag str = \\case
--             NodeElement e -> eltName e == str
--             _ -> False
-- @
--
-- Remember that if you need to find a node matching a predicate, but don't know at which level in the tree the node is located on, use Data.Foldable.find to find that node, then pass that node as a root to findByPath
findByPath :: [(a -> Bool)] -> Tree a -> Maybe (Tree a)
findByPath [] t = pure t
findByPath (p:ps) (Node {rootLabel, subForest})
    | p rootLabel = pure . Node rootLabel $ foldMap (maybeToList . findByPath ps) subForest
    | otherwise   = empty

-- | Remove branches whose root nodes match the given predicate
--
-- Similar to 'findByPath', except matches anywhere in a tree, is is inverse (i.e. predicate is for removing branches rather than preserving them)
--
-- Also always returns root of given tree (i.e. root is present in tree even if predicate fails; that's the closest we can get to returning an empty tree without using Maybe or [])
pruneTree :: (a -> Bool) -> Tree a -> Tree a
pruneTree p n@(Node {subForest}) = n {subForest = [pruneTree p c | c@(Node {rootLabel}) <- subForest, not $ p rootLabel]}

-- | @x `putInTree` y@ puts x into y's subForest
putInTree :: Tree a -> Tree a -> Tree a
putInTree a n@(Node {subForest = sf}) = n {subForest = sf <> [a]}
        
-- | Like 'findByPath', searches through tree for branch that matches string of predicates. Rather than returning a tree whose branches match the predicates, matchTree returns the deepest nodes matching all the predicates. For example, in the tree 5 {4, 3, 2}, matchTree [odd, odd] returns [3]. matchTree [odd, even] would return [4, 2]. Of course, matchTree [odd] returns [5].
--
-- This is basically a generalized CSS selector.
--
-- Remember that if you need to find a node matching a predicate, but don't know at which level in the tree the node is located on, use Data.Foldable.find (or ffilter) to find those node(s), then pass as a root to matchTree
matchTree :: [(a -> Bool)] -> Tree a -> [a]
matchTree [] _ = empty
matchTree [p] (Node {rootLabel}) = bool' (const empty) pure p rootLabel
matchTree (p:ps) (Node {rootLabel, subForest})
    | p rootLabel = foldMap (matchTree ps) subForest
    | otherwise   = empty

-- | Searches arbitrary depth, and takes only one predicate
--
-- We can't just use @pruneTree . (not.)@ because that doesn't support arbitrary-depth searching: suppose we want to find only \<p\> elements; then we prune all (/=\<p\>). This deletes non-\<p\> parents of \<p\> before we can search them for \<p\>'s.
--
-- Thus @selectBranches@ uses depth-first search.
--
-- Keep in mind that selectBranches returns parent-most matching nodes. For example: if you wanted to search for \<div\>'s, and some div's contained other div's, only the parent-most div would be included in the result list. That node still has subnodes in its branches, but if you want all nodes, you should do a fold of your own.
selectBranches :: (t -> Bool) -> Tree t -> [Tree t]
selectBranches p = go [] where go ret n@(Node {rootLabel, subForest}) = if p rootLabel then n:ret else foldMap (go ret) subForest

{- I don't even remember why I began writing this (or toPaths). I'll finish it whenever needed or if I feel like solviing it just for heckies.
fromPaths :: Eq a => NonEmpty (NonEmpty a) -> Tree a
fromPaths bs = foldr (\branch t -> _) (Node (NE.head $ NE.head bs) []) $ NE.drop 1 <$> bs
    where
        -- | A left-to-right list of nodes becomes a bottom-to-top hierarchy. The Int in the return value is the rightmost Int of the list (and is thus a minimum of the input list's fst's.)
        -- listToBranch [(8, pure "gc"), (4, pure "c"), (0, pure "p")] --> (0,
        -- ─ p
        --   └─ c
        --      └─ gc)
        -- Note that the following assertion should hold: ∀i j ∈ fst <$> inputList: i < j => inputList !! i > inputList !! j.
        -- We return the minimum because listToBranch represents folding a list into a (sub)tree; that subtree is rooted at a particular level; this level is what's returned.
        listToBranch :: [(i, Tree a)] -> (i, Tree a)
        listToBranch = maybe (error "Logic error in function readIndented(General): empty list passed to listToBranch. Please report this bug.") (uncurry $ foldl' (\(_,b) (i,a) -> (i, b `putInTree` a))) . uncons -}

-- | Convert a tree to a list of its branches. An embedding into M(n,m) space (a ragged array) for a tree with n levels and a maximum of m siblings across its levels.
toPaths :: Tree a -> NonEmpty (NonEmpty a)
toPaths = NE.fromList . fmap NE.fromList . go
    where
        go :: Tree a -> [[a]]
        go (Node r []) = pure (pure r) -- we need to specify a base case value because foldMap/foldr (and fmap) both do *nothing* (i.e. work with undefined) when predicated over the empty list! Thus our base case must not be [] :: [[Int]]; but instead, [[]] :: [[Int]]. The usual recursive function would see us doing (r:) <$> [[]], which equals [[r]], so [[r]] is what we return here.
        go (Node r cs) = (r:) <$> foldMap go cs

-- | Creates a directed graph from a tree.
toGraph :: Graph gr
        => Bool -- ^ whether to direct arrows from children to parent
        -> Bool -- ^ whether to direct arrows between siblings
        -> Tree a -- ^ tree to convert
        -> gr a Rel
toGraph c s = (\(_,vs,es) -> mkGraph vs es) . toGraph' 0 c s

-- | Create a graph with nodes starting from a given index. Useful for reading multiroot trees.
-- For instance, you may want a function that creates a graph from a multiroot tree:
--
-- @
-- multiTreeToGraph :: Graph gr => Text -> gr Text Rel
-- multiTreeToGraph = (\(_,x,y) -> mkGraph x y) . foldl' g (0,[],[]) . readIndentedText id
--     where
--         g b@(k,vs,es) = \case
--             Right t -> case toGraph' k False False t of
--                 (k',vs',es') -> (k', vs <> vs', es <> es')
--             _ -> b
-- @
--
-- Let's make a file called multiTree.txt:
--
-- @
-- node 1
--     node 2
--     node 3
--         node 4
--     node 5
-- node 2:1
--     node 2:2
--         node 2:5
--     node 2:3
--     node 2:4
-- @
--
-- And use it altogether with <http://hackage.haskell.org/package/fgl-visualize-0.1.0.1/docs/Data-Graph-Inductive-Dot.html fgl-visualize> - a package that generates graphviz dot files from fgl graphs. Below is my GHCi session:
--
-- @
-- :m + Text.Dot Data.Graph.Inductive.Dot Data.Graph.Inductive.Graph Data.Graph.Inductive.PatriciaTree
-- TIO.readFile "/home/nic/multiTree.txt" >>= writeFile "/home/nic/multiTree.gv" . showDot . (fglToDot :: Gr T'.Text Rel -> Dot ()) . multiTreeToGraph
-- @
--
-- Then in bash, @dot -Tsvg -O multiTree.gv@. This produces this lovely graphic:
--
-- ![multiTree.gv.svg](TODO: upload ~/multiTree.gv.svg to CloudFlare, then put that URL here)
--
-- Remember to pass @--package fgl-visualize --package dotgen@ if using @stack ghci@.
toGraph' :: Int -- ^ number nodes starting at this index
         -> Bool -- ^ whether to direct arrows from children to parent
         -> Bool -- ^ whether to direct arrows between siblings
         -> Tree a -- ^ tree to convert
         -> (Int, [LNode a], [LEdge Rel])
toGraph' i0 directUp directSibs = \(Node r rs) -> case go (i0 + 1, mempty, mempty, mempty) ((i0,) <$> rs) of
    (k, sibsMap, vs, es) -> (k, (i0, r):vs, M.foldl' (\edges children -> connect children <> edges) es sibsMap)
    where
        go :: t ~ ( Int
                  , M.Map Int [Int] -- pid -> associated children's node id
                  , [LNode a] -- vertex set
                  , [LEdge Rel] -- edge set
                  )
           => t
           -> [(Int, Tree a)] -- stack of: (parent node id, current node to traverse)
           -> t
        go x@(i@(succ -> j), sibsMap, vs, es) = \case
            [] -> x -- return the stuff
            (pid, Node r rs) : stack -> -- collect new stuff
                let vertex = (i, r) -- this node as a vertex
                    edges =
                        let cov = (pid, i, Child)
                            contrav = (i, pid, Parent)
                        in if directUp then [cov, contrav] else [cov]
                    map' = if directSibs then
                               case M.lookup pid sibsMap of
                                   Nothing -> M.insert pid [i] sibsMap
                                   Just _ -> M.update (pure . (i:)) pid sibsMap
                           else sibsMap
                in go (j, map', vertex:vs, edges <> es) $ ((i,) <$> rs) <> stack

        connect :: [Int] -> [LEdge Rel]
        connect cs = toList . S.fromList $ do -- TODO: don't use set to remove duplicates
            i <- cs
            j <- cs
            guard (i /= j)
            [(i,j,Sibling), (j,i,Sibling)]

-- | Create a text readable by 'readIndentedText'
showIndented :: (a -> T'.Text) -- ^ convert item to text
              -> Tree a
              -> T'.Text
showIndented toText = T'.init . go 0 -- init ∵ go leaves a trailing newline. Works fine on singleton trees, too.
    where
        go indentation (Node (toText -> a) xs) = mtimesDefault (4 * indentation) " " <> a <> "\n" <> foldMap (go (succ indentation)) xs

-- | Generalizes 'readIndented' by determining tree level by a given function rather than the number of spaces. Created for the @autoSections@ – and later @deriveTOC@ – functions in my @nixys-server-box@ package.
--
-- Parses multirooted trees into a @Seq@. Each root has a potential of failing to parse due to ambiguous indentation.
--
--  The return type is natural of the computation; use 'sequenceA' to convert to @Either Text (Seq (Tree b))@. If you do, consider that this @sequenceA@ is non-isomorphic, despite being a bijection.
readIndentedGeneral :: (Integral i, LL.ListLike t a)
                    => (a -> T'.Text) -- ^ function to convert input to strict text for output error messages (you may want to use a whitespace stripping function in there)
                    -> (a -> Either b (i, b)) -- ^ @Right@ (length, item to put into tree node); or @Left@ for elements that aren't a part of the tree structure, but need to be included nonetheless, /e.g./ incidental Html when parsing an HTML document. You'll know when you need to return in @Left@; if you don't understand, then you probably just don't need to worry about it, and should just return in @Right@.
                    -> t -- ^ input to process
                    -> Seq (Either T'.Text (Tree b))
readIndentedGeneral toL toLvl'R is = (\case Left l -> pure (Left l); Right (p, ts) -> go [p] ts) $ do -- latter (Left l) isn't equal in type to the (Left l) that was matched against!
    (root, ts) <- liftME "Cannot parse a tree from an empty list of nodes!" $ LL.uncons is
    p <- BiF.first (const "First line must parse into the tree's root; could not derive node level and value from first line.") $ textToPair root -- Nothing nodes are always siblings ↔ Nothing nodes must have a parent ↔ a Nothing node cannot be a root
    pure (p, ts)
    where
        -- a trailing prime on variable names (e.g. i') denotes that of the current step; the same variable name without the prime denotes the prior step's values
        -- Mnemonics: t(ree), i(ndentation), p(air), s(stack). Note that variable "text" is used ONLY for returning error messages! p' is the form of text that's actually used in computation!
        -- Because of listToBranch, pushing a node onto the stack corresponds to increasing the depth of the tree by 1
        go s@((i,t):ps) u = case LL.uncons u of
            Nothing -> close (pure s)
            Just (text, ts) ->
                case textToPair text of
                    Left l -> go ((i, pure l `putInTree` t):ps) ts -- put moot node as child of stack's top node. Don't modify the current indentation.
                    Right p'@(i',_) -> let nipN'Plant = close (pure s) <> go [p'] ts in case compare i' i of
                        GT -> go (p':s) ts -- current text is more indented than prior text; push this pair to the stack
                        EQ -> case ps of -- EQ means that p is a single-node/childless tree; because it won't have any children, we can assuredly pop it off the stack and fold it into its parent!
                            [] -> nipN'Plant
                            (parent:ancestors) -> go (p' : second (t `putInTree`) parent : ancestors) ts -- second (t `putInTree`) parent = listToBranch [p, parent], but more efficient
                        LT -> case span ((/=i') . fst) s of
                            (_,[]) -> pure . Left $ "Improperly formatted list: a node with text value \"" <> toL text <> "\" should have the same indentation as some previous node, but it doesn't."
                            (a, b:bs) -> case (a <> [b], bs) of -- we need to go one further than span, to include the last sibling of p'
                                (_,[]) -> nipN'Plant -- like the entrypoint to readIndentedGeneral, except that we know that we're given a list starting with a Right-like element
                                (listToBranch -> (_,z), parent:ancestors) -> go (p' : second (z `putInTree`) parent : ancestors) ts
                                           -- this^ blank should equal i'

        -- zip-up b [(i, Tree b)] and pull the tree out
        close :: Either T'.Text [(i, Tree b)] -> Seq (Either T'.Text (Tree b))
        close = pure . fmap (snd . listToBranch)

        -- | (indention_level, Node text [])
        textToPair = fmap (second pure) . toLvl'R

        -- | A left-to-right list of nodes becomes a bottom-to-top hierarchy. The Int in the return value is the rightmost Int of the list (and is thus a minimum of the input list's fst's.)
        -- listToBranch [(8, pure "gc"), (4, pure "c"), (0, pure "p")] --> (0,
        -- ─ p
        --   └─ c
        --      └─ gc)
        -- Note that the following assertion should hold: ∀i j ∈ fst <$> inputList: i < j => inputList !! i > inputList !! j.
        -- We return the minimum because listToBranch represents folding a list into a (sub)tree; that subtree is rooted at a particular level; this level is what's returned.
        listToBranch :: [(i, Tree a)] -> (i, Tree a)
        listToBranch = maybe (error "Logic error in function readIndented(General): empty list passed to listToBranch. Please report this bug to <nicholas.chandoke@gmail.com>.") (uncurry $ foldl' (\(_,b) (i,a) -> (i, b `putInTree` a))) . uncons

-- | Reads (multirooted) trees encoded in indented text /e.g./
--
-- @
-- parent1
--     child1
--     child2
--         grandchild1
--     child3
-- parent2
--     child4
--     child5
--         grandchild2
--    ⋮
-- @
--
-- Must be in either tabs or spaces exclusively; any mixture of leading tabs and spaces results in undefined behavior.
--
-- Returns either a parsing error message or a tree
--
-- Note that 'readIndented' morphism and input arguments are lazy text, but it returns strict text in its Left constructor.
readIndentedText :: (T'.Text -> a) -> T'.Text -> Seq (Either T'.Text (Tree a))
readIndentedText toR = readIndentedGeneral (T'.stripStart) (pure . (T'.length *** toR) . T'.span isSpace) . T'.lines

-- | Lazy text version of 'readIndentedText'
readIndentedTextLazy :: (T.Text -> a) -> T.Text -> Seq (Either T'.Text (Tree a))
readIndentedTextLazy toR = readIndentedGeneral (T'.stripStart . T.toStrict) (pure . (T.length *** toR) . T.span isSpace) . T.lines
