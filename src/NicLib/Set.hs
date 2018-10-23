module NicLib.Set
( setFind
, partitionBy
, partitionBy'
, diff
) where
import NicLib.NStdLib
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Vector as V
import qualified Data.Bifunctor as BiF

-- | return minimum element matching a predicate
setFind :: Ord a => (a -> Bool) -> Set a -> Maybe a
setFind = cT (bool' (Just . S.elemAt 0) (const Nothing) S.null) S.filter

-- | Group things by a predicate whose domain and codomain both permit an ordering
-- Partition by the first letter:
-- @partitionBy head ["aardvark", "apple", "banana", "baby", "candy", "dragon", "dragonite", "daring", "zoo", "zooology"]@
-- or, @partitionBy fileExtension <$> listContents "."@ (these functions in NicLib.FileSystem)
-- this is a generalization of Data.List.partition; Data.List.partition a2Bool = partitionBy a2Bool (at least when passing a list rather than a Foldable...)
partitionBy :: (Foldable t, Ord a, Ord b) => (a -> b) -> t a -> V.Vector (b, Set a)
partitionBy f = foldr (\a sets -> maybe (V.cons (f a, S.singleton a) sets) (\i -> sets V.// [(i, BiF.second (S.insert a) (sets V.! i))]) $ V.findIndex ((== f a) . fst) sets) V.empty

-- | requires an extra Vector to Set conversion, but gives a set
partitionBy' :: (Foldable t, Ord a, Ord b) => (a -> b) -> t a -> Set (OrderBy b (Set a))
partitionBy' = cT (foldMap (S.singleton . OrderBy)) partitionBy

-- | diff a b = (a \ b, b \ a, a ∩ b)
-- Note that diff's runtime efficiency is linear in its first argument, and is not affected by the size of its second argument!
-- Thus, if you know that one set is smaller than the other, it should go as the first argument!
diff :: Ord a => Set a -> Set a -> (Set a, Set a, Set a)
diff a b = S.foldr (\x (ja, jb, ovrlp) -> if x `S.member` b then (ja, S.delete x jb, S.insert x ovrlp) else (S.insert x ja, jb, ovrlp)) (S.empty, b, S.empty) a
