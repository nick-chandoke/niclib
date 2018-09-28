module NicLib.Set
( setFind
, partitionBy
, partitionBy'
) where
import NicLib.NStdLib
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Bifunctor as BiF

-- | return minimum element matching a predicate
setFind :: Ord a => (a -> Bool) -> S.Set a -> Maybe a
setFind = cT (bool' (Just . S.elemAt 0) (const Nothing) S.null) S.filter

-- | Group things by a predicate whose domain and codomain both permit an ordering
-- Partition by the first letter:
-- @partitionBy head ["aardvark", "apple", "banana", "baby", "candy", "dragon", "dragonite", "daring", "zoo", "zooology"]@
-- or, @partitionBy fileExtension <$> listContents "."@ (these functions in NicLib.FileSystem)
-- this is a generalization of Data.List.partition; Data.List.partition a2Bool = partitionBy a2Bool (at least when passing a list rather than a Foldable...)
partitionBy :: (Foldable t, Ord a, Ord b) => (a -> b) -> t a -> V.Vector (b, S.Set a)
partitionBy f = foldr (\a sets -> maybe (V.cons (f a, S.singleton a) sets) (\i -> sets V.// [(i, BiF.second (S.insert a) (sets V.! i))]) $ V.findIndex ((== f a) . fst) sets) V.empty

-- | requires an extra Vector to Set conversion, but gives a set
partitionBy' :: (Foldable t, Ord a, Ord b) => (a -> b) -> t a -> S.Set (OrderBy b (S.Set a))
partitionBy' = cT (foldMap (S.singleton . OrderBy)) partitionBy
