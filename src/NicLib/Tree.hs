module NicLib.Tree
( DoubleTree
, doubleTree
, findByPath
, pruneTree
, matchTree
, selectBranches
, readIndented
) where

import Data.Char (isSpace)
import Data.Foldable
import Data.Graph -- containers
import Data.Int (Int64)
import Data.Tree
import NicLib.Errors (liftME, err)
import NicLib.NStdLib ((>>>), (***), (>=>), foldM, empty, bool', uncons, maybeToList, second)
import qualified Data.Text as T'
import qualified Data.Text.Lazy as T

{-
data Rel = Child | Sibling | Parent deriving (Show, Eq, Ord)
instance As (Graph Rel a) where
    type To (Graph a) = Tree a
    as = 
-}

-- TODO: deprecate in favor of as @(Graph a) (see above)
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
-- Must be in either tabs or spaces exclusively; any mixture of leading tabs and spaces results in undefined behavior.
-- Returns either a parsing error message or a tree
-- Note that @readIndented@ morphism and input arguments are lazy text, but it returns strict text in its Left constructor.
readIndented :: (T.Text -> a) -> T.Text -> Either T'.Text (Tree a) -- TODO: use conduit instead of foldM (in fact, since it's lazy text, it should be a lazy foldr!)
readIndented f = (T.lines >>> liftME "readIndented: cannot parse a tree from an empty list of nodes!" . uncons)
              >=> \(textToPair -> p, ts) -> snd . listToBranch <$> foldM go [p] ts
    where
--      go :: [(Int64, Tree a)] -> T.Text -> Either T'.Text [(Int64, Tree a)] -- existential crisis if uncomment
        -- stuff with prime (e.g. i') denotes the current step; the same variable name without the prime denotes the prior step's values
        -- Mnemonics: t(ree), i(ndentation), p(air), s(stack). Note that variable "text" is used ONLY for returning error messages! p' is the form of text that's actually used in computation!
        -- Because of listToBranch, pushing a node onto the stack corresponds to increasing the depth of the tree by 1
        go s@((i,t):ps) text@(textToPair -> p'@(i',_)) = case compare i' i of -- ignore non-exhaustive pattern-match warning; initializing go with [p] above
            GT -> pure $ p':s -- current text is more indented than prior text; push this pair to the stack
            EQ -> case ps of -- EQ means that p is a single-node/childless tree; because it won't have any children, we can assuredly pop it off the stack and fold it into its parent!
                [] -> Left $ "Tree must have exactly one root; \"" <> T'.stripStart (T.toStrict text) <> "\" cannot also be a root."
                (parent:ancestors) -> pure $ p' : second (t `putInTree`) parent : ancestors -- second (t `putInTree`) parent = listToBranch [p, parent], but more efficient
            LT -> case span ((/=i') . fst) s of
                (_,[]) -> Left $ "Improperly formatted list: a node with text value \"" <> T'.stripStart (T.toStrict text) <> "\" should have the same indentation as some previous node, but it doesn't."
                (a, b:bs) -> case (a <> [b], bs) of -- we need to go one further than span, to include the last sibling of p'
                                                    -- again, ignore inexhaustive pattern match warning; (_,[]) is matched on the above line
                    (listToBranch -> (_,z), parent:ancestors) -> pure $ p' : second (z `putInTree`) parent : ancestors
                                   -- ^ should equal i'

        -- | (indention, Node text [])
        -- textToPair :: T.Text -> (Int64, Tree a) -- existential crisis if uncomment
        textToPair = (T.length *** pure . f) . T.span isSpace
        
        -- | @x `putInTree` y@ puts x into y's subForest
        putInTree :: Tree a -> Tree a -> Tree a
        putInTree a n@(Node {subForest = sf}) = n {subForest = sf <> [a]}
        
        -- | A left-to-right list of nodes becomes a bottom-to-top hierarchy. The Int in the return value is the rightmost Int of the list (and is thus a minimum of the input list's fst's.)
        -- listToBranch [(8, pure "gc"), (4, pure "c"), (0, pure "p")] --> (0,
        -- ─ p
        --   └─ c
        --      └─ gc)
        -- Note that the following assertion should hold: ∀i j ∈ fst <$> inputList: i < j => inputList !! i > inputList !! j.
        -- We return the minimum because listToBranch represents folding a list into a (sub)tree; that subtree is rooted at a particular level; this level is what's returned.
        listToBranch :: [(Int64, Tree a)] -> (Int64, Tree a)
        listToBranch = maybe (error "Logic error in function readIndented: empty list passed to listToBranch. Please report this bug.") (uncurry $ foldl' (\(_,b) (i,a) -> (i, b `putInTree` a))) . uncons
