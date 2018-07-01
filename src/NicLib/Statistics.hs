{-# LANGUAGE
  OverloadedLists
, TupleSections
, ViewPatterns
#-}

module NicLib.Statistics
( randDist
, showHist
, toHist
, toHist'
, showHist'
, printRandDist
) where

import Data.Random
import Data.Random.Distribution.Exponential
import Data.Random.Source.DevRandom
import NicLib.NStdLib
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Vector as V

-- module useful for exploring statistical distributions

printRandDist :: Int -> RVar Double -> IO ()
printRandDist i d = randDist i d >>= TLIO.putStrLn . showHist . toHist Nothing

-- | a random distribution, i.e. a set of randomly generated floats, of cardinality n
-- e.g. randDist 1300 (exponential 16)
randDist :: (MonadIO m, Ord a) => Int -> RVar a -> m (S.Set a)
randDist i = liftIO . replicateM' i . fmap S.singleton . flip runRVar DevURandom -- DevURandom generates pseudorandom numbers when the entropy pool is dry

-- | Horizontal text display of a histogram
-- TODO: scale width of histogram output to number of columns
-- TODO: change floor to round to n digits
showHist :: (Foldable t, RealFrac n, Show n) => t ((n, n), Int) -> T.Text
showHist xs = case getMax $ foldMap (Max . length . printRange . both floor . fst) xs of -- actually, the last element should be the longest, assuming all elements have the same number of decimal digits
    (fromIntegral -> maxInXS) -> T.unlines $ foldMap (\((T.pack &&& fromIntegral . length) . printRange . both floor -> (t, l), fromIntegral -> numInBin) -> [t <> (T.take (maxInXS - l) (T.repeat ' ')) <> T.cons ' ' (T.take numInBin (T.repeat '◼'))]) xs
    where
        printRange (a, b) = show a ++ '~':(show b)

showHist' :: (Foldable t, Show a) => t (a, Int) -> T.Text
showHist' xs = case getMax $ foldMap (Max . length . show . fst) xs of -- actually, the last element should be the longest, assuming all elements have the same number of decimal digits
    (fromIntegral -> maxInXS) -> T.unlines $ foldMap (\((T.pack &&& fromIntegral . length) . show -> (t, l), fromIntegral -> numInBin) -> [t <> (T.take (maxInXS - l) (T.repeat ' ')) <> T.cons ' ' (T.take numInBin (T.repeat '◼'))]) xs

-- number of bins is determined by variance of distribution if Nothing is passed; else one may manually specify the number of bins
-- number of bins may be fewer than you requested, if the top bins aren't filled
-- retuns a set of ranges and the number of values falling in those ranges
-- TODO: how to automatically determine bin size?
-- BUG: hangs when given randDist 1 (exponential _), even though that expression itself evaluates fine
toHist :: (RealFloat n, Foldable t) => Maybe Int -> t n -> S.Set ((n, n), Int)
toHist (fromMaybe 20 -> numBins) dist = case (uncurry (-) . (getMax' *** getMin') $ foldMap (Max' &&& Min') dist) / fromIntegral numBins of -- binSize :: n
    binSize -> S.map (first ((id &&& (+binSize)) . (*binSize) . fromIntegral)) $ foldMap (S.singleton . second S.size) $ partitionBy (floor . (/binSize)) dist -- partition/sort data into bins, then replace bin indicies with value ranges

newtype Min' a = Min' {getMin' :: a}
newtype Max' a = Max' {getMax' :: a}

instance (Num a, Ord a) => Semigroup (Min' a) where Min' a <> Min' b = Min' (min a b)
instance (Num a, Ord a) => Monoid (Min' a) where mempty = Min' (fromInteger 0)

instance (Num a, Ord a) => Semigroup (Max' a) where Max' a <> Max' b = Max' (max a b)
instance (Num a, Ord a) => Monoid (Max' a) where mempty = Max' (fromInteger 0)

-- | histogram where bins are single values rather than ranges, i.e. we count occurrences of unique values
toHist' :: (Ord n, Foldable t) => t n -> S.Set (n, Int) -- I could change Ord n to Eq n, and S.Set (n, Int) to [(n, Int)]
toHist' = S.map untagPair . foldr (\a b -> maybe (S.insert (TagPair (a, 1)) b) (flip S.insert b . TagPair . second succ) $ setFind (==a) b) S.empty
    where
        setFind :: (a -> Bool) -> S.Set (TagPair a b) -> Maybe (a, b)
        setFind p = fmap untagPair . listToMaybe . S.toList . S.filter (p . fst . untagPair)

-- next steps: generalize: use partitionBy more generally, and implement a system for composing filters combinatorially, so that one can filter in a venn-diagram style, or sort-through data to whittle-down to what they're looking for (building a scraper incrementially. remember that a scraper is merely a thing that identifies a particular subset from a given set (the given set will likely have to satisfy some predicates that validate the sensibility of the scraper.)
