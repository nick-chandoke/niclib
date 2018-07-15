{-# LANGUAGE NamedFieldPuns #-}
module NicLib.Tree
( DoubleTree
, findByPath
, matchTree
, selectBranches
, doubleTree
, pruneTree
) where
import NicLib.NStdLib
import Data.Tree

-- | a Tree (Tree a, a) allows all the Functor, Traversable &c instances for Tree. DoubleTree is isomorphic with the type (Tree a, a), however; we can traverse up the tree by recursively using the first element (parent tree) of the tuple, and so navigate the tree completely. I have DoubleTree as a Tree around the tuple simply because it allows all the functionality that the Tree module provides. I could easily derive similar or parallel instances or methods for the (Tree a, a) type, but currently I have no need.
type DoubleTree a = Tree (Tree a, a) -- doubly-linked Tree; each child/node has a reference to its parent

-- | doubly-links a tree, so that each child tree carries a reference to its parent
-- the first parameter is a dummy value for the root node, and is used in recursion for all other nodes
-- useful in filters: ffilter (\(parent, value) -> ...) (doubleTree dummy tree); remember that Tree is a Foldable, and the 2-tuple (Tree a, a) is the type of the argument to ffilter
doubleTree :: a -> Tree a -> Tree (Tree a, a)
doubleTree pv p@(Node v children) = Node (Node pv [], v) (dt p <$> children)
    where 
        dt :: Tree a -> Tree a -> Tree (Tree a, a)
        dt parent (Node v children) = Node (parent, v) (dt parent <$> children)

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
