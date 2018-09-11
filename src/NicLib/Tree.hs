{-# LANGUAGE NamedFieldPuns, RankNTypes, LambdaCase, FlexibleContexts #-}

module NicLib.Tree
( DoubleTree
, doubleTree
, findByPath
, pruneTree
, matchTree
, selectBranches
, readIndented
) where

import NicLib.NStdLib
import Data.Tree
import Data.Char (isSpace)
import Data.Foldable
import qualified Data.ListLike as LL
import Data.ListLike.String (StringLike, toString)
import Data.Graph -- containers

-- | a Tree (Tree a, a) allows all the Functor, Traversable &c instances for Tree. DoubleTree is isomorphic with the type (Tree a, a), however; we can traverse up the tree by recursively using the first element (parent tree) of the tuple, and so navigate the tree completely. I have DoubleTree as a Tree around the tuple simply because it allows all the functionality that the Tree module provides. I could easily derive similar or parallel instances or methods for the (Tree a, a) type, but currently I have no need.
type DoubleTree a = Tree (Tree a, a) -- doubly-linked Tree; each child/node has a reference to its parent

-- | doubly-links a tree, so that each child tree carries a reference to its parent
-- the first parameter is a dummy value for the root node, and is used in recursion for all other nodes
-- useful in filters: ffilter (\(parent, value) -> ...) (doubleTree dummy tree); remember that Tree is a Foldable, and the 2-tuple (Tree a, a) is the type of the argument to ffilter
doubleTree :: a -> Tree a -> DoubleTree a
doubleTree pv p@(Node v children) = Node (Node pv [], v) (dt p <$> children)
    where 
        dt :: Tree a -> Tree a -> Tree (Tree a, a)
        dt parent (Node v children) = Node (parent, v) (dt parent <$> children)

{-
data Rel = Child | Sibling | Parent deriving (Show, Eq, Ord)

toGraph :: Tree a -> Graph
toGraph = 
-}

-- | returns branches matching the ordered list of predicates.
-- returns Nothing if root fails to match. Note that the maximum depth of the returned tree is the length of the predicate list; to take all children past a given depth, use repeat, e.g. to get a Tree consisting of all branches from <!doctype> to descendants of <head>: findByPath (((\str -> \case NodeElement e -> eltName e == str; _ -> False) <$> ["DOCTYPE", "html", "head"]) ++ repeat (const True)) tree
-- remember that if you need to find a node matching a predicate, but don't know at which level in the tree the node is located on, use Data.Foldable.find to find that node, then pass that node as a root to findByPath
findByPath :: [(a -> Bool)] -> Tree a -> Maybe (Tree a)
findByPath [] t = pure t
findByPath (p:ps) (Node {rootLabel, subForest})
    | p rootLabel = pure . Node rootLabel $ foldMap (maybeToList . findByPath ps) subForest
    | otherwise   = empty

-- | remove branches whose root nodes match the given predicate
-- similar to findByPath, except matches anywhere in a tree, is is inverse (i.e. predicate is for removing branches rather than preserving them)
-- also always returns root of given tree (i.e. root is present in tree even if predicate fails; that's the closest we can get to returning an empty tree without using Maybe or [])
pruneTree :: (a -> Bool) -> Tree a -> Tree a
pruneTree p n@(Node {subForest}) = n {subForest = [pruneTree p c | c@(Node {rootLabel}) <- subForest, not $ p rootLabel]}

-- | like findByPath, searches through tree for branch that matches string of predicates. Rather than returning a tree whose branches match the predicates, matchTree returns the deepest nodes matching all the predicates. For example, in the tree 5 {4, 3, 2}, matchTree [odd, odd] returns [3]. matchTree [odd, even] would return [4, 2]. Of course, matchTree [odd] returns [5].
-- this is basically a generalized CSS selector
-- remember that if you need to find a node matching a predicate, but don't know at which level in the tree the node is located on, use Data.Foldable.find (or ffilter) to find those node(s), then pass as a root to matchTree
matchTree :: [(a -> Bool)] -> Tree a -> [a]
matchTree [] _ = empty
matchTree [p] (Node {rootLabel}) = bool' (const empty) pure p rootLabel
matchTree (p:ps) (Node {rootLabel, subForest})
    | p rootLabel = foldMap (matchTree ps) subForest
    | otherwise   = empty

-- | searches arbitrary depth, and takes only one predicate
-- we can't just use (pruneTree . (not.)) because that doesn't support arbitrary-depth searching: suppose we want to find only <p> elements; then we prune all (/=<p>). This deletes non-<p> parents of <p> before we can search them for <p>'s.
-- thus selectBranches uses depth-first search
-- keep in mind that selectBranches returns parent-most matching nodes. For example: if you wanted to search for <div>'s, and some div's contained other div's, only the parent-most div would be included in the result list. That node still has subnodes in its branches, but if you want all nodes, you should do a fold of your own.
selectBranches :: (t -> Bool) -> Tree t -> [Tree t]
selectBranches p = go [] where go ret n@(Node {rootLabel, subForest}) = if p rootLabel then n:ret else foldMap (go ret) subForest

-- | reads structures of form
-- parent
--     child1
--     child2
--         grandchild1
--     child3
--    ⋮
-- must be in either tabs or spaces exclusively; any mixture of leading tabs and spaces results in undefined behavior.
readIndented :: (LL.ListLike str Char, StringLike str) => (str -> a) -> str -> Tree a
readIndented f = (LL.lines :: forall str. StringLike str => str -> [str]) >>> LL.uncons >>> \case
    Nothing -> error "readIndented: cannot parse a tree from an empty list of nodes!"
    Just (t,ts) -> snd . listToBranch $ foldl' go [textToPair t] ts
    where
--      go :: [(Int, Tree a)] -> str -> [(Int, Tree a)]
        go s@(p@(i,v):ns) text =
            let p'@(i',_) = textToPair text
            in case compare i' i of
                GT -> p':s
                EQ -> case ns of
                    (a:as) -> p':fmap (putInTree v) a:as
                    _ -> (error "improperly formatted tree: multiple nodes on the same level must share a common parent. this condition is violated.")
                LT -> case span ((/=i') . fst) s of
                    (_,[]) -> error $ "readIndented: improperly formatted list: the node \"" ++ toString text ++ "\" should have the same indentation as some previous node, but it doesn't."
                    (zipUs, don'tZipUs) -> listToBranch zipUs : don'tZipUs

        textToPair = (LL.length *** pure . f) . LL.span isSpace

        -- | add a tree into another tree's subForest
        putInTree :: Tree a -> Tree a -> Tree a
        putInTree a n@(Node {subForest = f}) = n {subForest = f `LL.snoc` a}

        -- | a left-to-right list of nodes becomes a bottom-to-top hierarchy. The Int in the return value is the rightmost Int of the list
        listToBranch :: [(Int, Tree a)] -> (Int, Tree a)
        listToBranch = maybe (error "listToBranch: empty list") (uncurry $ foldl' (\(_,b) (i,a) -> (i, putInTree b a))) . uncons
