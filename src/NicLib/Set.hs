module NicLib.Set
( setFind
, partitionBy
, partitionBy'
, diff
, showDiff
) where

import Data.Align (Align, padZipWith)
import Data.Foldable (maximum)
import Data.Maybe (fromMaybe)
import NicLib.NStdLib
import RIO
import RIO.Set (Set)
import qualified Data.Bifunctor as BiF
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified RIO.Text as T'

-- | return minimum element matching a predicate
setFind :: Ord a => (a -> Bool) -> Set a -> Maybe a
setFind = cT (bool' (Just . S.elemAt 0) (const Nothing) S.null) S.filter

-- TODO: I should change partitionBy to return (ListLike full a) => Vector (b, full), rather than Vector (b, Set a); this requires a to implement Ord, but that's not a sensible restriction!
-- | Group things by a predicate whose domain and codomain both permit an ordering
-- Partition by the first letter:
-- @partitionBy head ["aardvark", "apple", "banana", "baby", "candy", "dragon", "dragonite", "daring", "zoo", "zooology"]@
-- or, @partitionBy fileExtension <$> listContents "."@ (these functions in NicLib.FileSystem)
-- this is a generalization of Data.List.partition; Data.List.partition a2Bool = partitionBy a2Bool (at least when passing a list rather than a Foldable...)
partitionBy :: (Foldable t, Ord a, Ord b) => (a -> b) -> t a -> V.Vector (b, Set a)
partitionBy f = foldr (\a sets -> maybe (V.cons (f a, S.singleton a) sets) (\i -> sets `V.unsafeUpd` [(i, BiF.second (S.insert a) (sets `V.unsafeIndex` i))]) $ V.findIndex ((== f a) . fst) sets) V.empty

-- | requires an extra Vector to Set conversion, but gives a set
partitionBy' :: (Foldable t, Ord a, Ord b) => (a -> b) -> t a -> Set (OrderBy b (Set a))
partitionBy' = cT (foldMap (S.singleton . OrderBy)) partitionBy

-- | diff a b = (a \\ b, b \\ a, a ∩ b)
-- Note that diff's runtime efficiency is linear in its first argument, and is not affected by the size of its second argument!
-- Thus, if you know that one set is smaller than the other, it should go as the first argument!
diff :: Ord a => Set a -> Set a -> (Set a, Set a, Set a)
diff a b = S.foldr (\x (ja, jb, ovrlp) -> if x `S.member` b then (ja, S.delete x jb, S.insert x ovrlp) else (S.insert x ja, jb, ovrlp)) (S.empty, b, S.empty) a

-- TODO: can be more efficient (multiple traversals of lists can be merged)
--       also I could reformulate this to calculate by-column rather than folding columns together, then computing after each fold
--          notably using longestC is just plain lazy; I should calculate that using sums of columns
-- | Converts a diff of @Text@s into a table, ready for pretty display in a terminal
showDiff :: T'.Text -- ^ column 1 heading
         -> T'.Text -- ^ column 3 heading
         -> (Set T'.Text, Set T'.Text, Set T'.Text) -- ^ diffs
         -> T'.Text
showDiff h1 h2 ( (h1:) . S.toList -> a
               , (h2:) . S.toList -> b
               , ("Both":) . S.toList -> c) =
    let longestA = longestLen a
        ab = mergeLists longestA padding a b
        longestAB = longestLen ab
        abc = mergeLists longestAB padding ab c
        longestC = longestLen abc
    in case abc of
        (h:ts) -> T'.unlines (h:T'.replicate longestC "─":ts)
  where
    padding :: Int
    padding = 3

    longestLen :: [T'.Text] -> Int
    longestLen = maximum . map T'.length

    -- notably an endomorphism
    mergeLists :: (Align f) => Int -> Int -> f T'.Text -> f T'.Text -> f T'.Text
    mergeLists ll padding = padZipWith (\(fromMaybe mempty -> a) (fromMaybe mempty -> b) -> a <> T'.replicate (ll - T'.length a + padding) " " <> b)
