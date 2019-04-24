-- | Read @Foldable@s into trees, and convert trees to graphs.
module NicLib.Tree
( Rel(..)
-- * Utilities
, toGraph
, toGraph'
-- * Morphisms from Tree
, showIndented
-- * Morphisms to Tree
, readIndentedGeneral
, readIndentedText
) where

import RIO
import Prelude (succ)

-- base
import Control.Arrow ((***), second, (|||), (>>>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Foldable
import Data.List (uncons)
import Data.Semigroup (mtimesDefault)

-- containers
import RIO.Seq (Seq)
import Data.Tree
import qualified RIO.Set as S
import qualified RIO.Map as M

-- NicLib
import NicLib.NStdLib (liftME)

-- misc.
import qualified Data.Bifunctor as BiF -- bifunctors
import qualified Data.ListLike as LL -- ListLike

-- text
import qualified Data.Text as T

-- fgl
import Data.Graph.Inductive.Graph hiding (empty)

data Rel = Child | Sibling | Parent deriving (Show, Eq, Ord)

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
-- TIO.readFile "/home/nic/multiTree.txt" >>= writeFile "/home/nic/multiTree.gv" . showDot . (fglToDot :: Gr T.Text Rel -> Dot ()) . multiTreeToGraph
-- @
--
-- Then in bash, @dot -Tsvg -O multiTree.gv@. This produces this lovely graphic:
--
-- ![multiTree.gv.svg](https://github.com/nick-chandoke/niclib/blob/master/multiTree.gv.svg)
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
        connect cs = toList . S.fromList $ do -- TODO: don't use lifting into set to remove duplicates; adjust the algorithm instead
            i <- cs
            j <- cs
            guard (i /= j)
            [(i,j,Sibling), (j,i,Sibling)]

-- | Create a text readable by 'readIndentedText'
showIndented :: (a -> T.Text) -- ^ convert item to text
             -> Tree a
             -> T.Text
showIndented toText = T.init . go 0 -- init ∵ go leaves a trailing newline. Works fine on singleton trees, too.
    where
        go indentation (Node (toText -> a) xs) = mtimesDefault (4 * indentation) " " <> a <> "\n" <> foldMap (go (succ indentation)) xs

-- types used in readIndentedGeneral
type LvlNode i a = (i, Tree a)

-- | Generalizes 'readIndented' by determining tree level by a given function rather than the number of spaces.
--
-- Parses multirooted trees into a @Seq@. Each root has a potential of failing to parse due to ambiguous indentation.
--
--  The return type is natural of the computation; use 'sequenceA' to convert to @Either Text (Seq (Tree b))@. If you do, consider that this @sequenceA@ is non-isomorphic, despite being a bijection.
readIndentedGeneral :: (Integral i, LL.ListLike t a)
                    => (a -> T.Text) -- ^ function to convert input to strict text for output error messages (you may want to use a whitespace stripping function in there)
                    -> (a -> Either b (i, b)) -- ^ @Right@ (length, item to put into tree node); or @Left@ for elements that aren't a part of the tree structure, but need to be included nonetheless, /e.g./ when parsing an HTML document's heading (e.g. h1, h2) elements into a tree, there may be html in-between that doesn't affect the heading hierarchy, that you nonetheless want to include in the tree; these non-hierarchy-affecting elements inherit the last-read hierarchy level.
                    -> t -- ^ input to process
                    -> Seq (Either T.Text (Tree b))
readIndentedGeneral toErrMsg ((fmap (second pure) .) -> textToPair) is = pure . Left ||| uncurry go . BiF.first pure $ do
    (root, ts) <- liftME "Cannot parse a tree from an empty list of nodes!" $ LL.uncons is
    p <- BiF.first (const "First line must parse into the tree's root; could not derive node level and value from first line.") $ textToPair root -- Nothing nodes are always siblings ↔ Nothing nodes must have a parent ↔ a Nothing node cannot be a root
    pure (p, ts)
    where
        -- a trailing prime on variable names (e.g. i') denotes that of the current step; the same variable name without the prime denotes the prior step's values
        -- Mnemonics: t(ree), i(ndentation), p(air), s(stack). Note that variable "text" is used ONLY for returning error messages! p' is the form of text that's actually used in computation!
        -- Because of listToBranch, pushing a node onto the stack corresponds to increasing the depth of the tree by 1
        go s@((i,t):ps) = LL.uncons >>> \case
            Nothing -> close s
            Just (text, ts) ->
                case textToPair text of
                    Left l -> go ((i, pure l `putInTree` t):ps) ts -- put moot node as child of stack's top node. Don't modify the current indentation.
                    Right p'@(i',_) -> let nipN'Plant = close s <> go [p'] ts in case compare i' i of
                        GT -> go (p':s) ts -- current text is more indented than prior text; push this pair to the stack
                        EQ -> case ps of -- EQ means that p is a single-node/childless tree; because it won't have any children, we can assuredly pop it off the stack and fold it into its parent!
                            [] -> nipN'Plant
                            (parent:ancestors) -> go (p' : second (t `putInTree`) parent : ancestors) ts -- second (t `putInTree`) parent = listToBranch [p, parent], but more efficient
                        LT -> case span ((/=i') . fst) s of
                            (_,[]) -> pure . Left $ "Improperly formatted list: a node with value \"" <> toErrMsg text <> "\" should have the same indentation as some previous node, but it doesn't."
                            (a, b:bs) -> case (a <> [b], bs) of -- we need to go one further than span, to include the last sibling of p'
                                (_,[]) -> nipN'Plant -- like the entrypoint to readIndentedGeneral, except that we know that we're given a list starting with a Right-like element
                                (listToBranch -> (_,z), parent:ancestors) -> go (p' : second (z `putInTree`) parent : ancestors) ts
                                           -- this^ blank should equal i'

        close :: [LvlNode i b] -> Seq (Either T.Text (Tree b))
        close = pure . fmap (snd . listToBranch) . pure

        -- | A left-to-right list of nodes becomes a bottom-to-top hierarchy. The terminal object's integral is the rightmost integral of the list (and is thus a minimum of the input list's fst's.)
        -- listToBranch [(8, pure "gc"), (4, pure "c"), (0, pure "p")] --> (0,
        -- ─ p
        --   └─ c
        --      └─ gc)
        -- We return the minimum because listToBranch represents folding a list into a (sub)tree; that subtree is rooted at a particular level; this level is what's returned.
        listToBranch :: [LvlNode i a] -> LvlNode i a
        listToBranch = maybe (error "Logic error in function readIndented(General): empty list passed to listToBranch. Please report this bug to <nicholas.chandoke@gmail.com>.") (uncurry $ foldl' (\(_,b) (i,a) -> (i, b `putInTree` a))) . uncons

        -- | @x `putInTree` y@ puts x into y's subForest
        putInTree :: Tree a -> Tree a -> Tree a
        putInTree a n@(Node {subForest = sf}) = n {subForest = sf <> [a]}

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
readIndentedText :: (T.Text -> a) -> T.Text -> Seq (Either T.Text (Tree a))
readIndentedText toR = readIndentedGeneral (T.stripStart) (pure . (T.length *** toR) . T.span isSpace) . T.lines
