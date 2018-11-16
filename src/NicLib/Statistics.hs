-- | Module useful for exploring statistical distributions
--
-- Much of this module is deprecated, as it's about outputting distribution graphics as text. This is stupid; it should be viewed via HTML, probably leveraging Lucid or PureScript!
module NicLib.Statistics
( randDist
, showHist
, toHist
, toHist'
, showHist'
, printRandDist
) where

import Control.Arrow ((&&&), (***), first, second)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad.IO.Class
import Data.Random
import Data.Random.Source.DevRandom
import NicLib.NStdLib
import NicLib.Set
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TLIO

printRandDist :: Int -> RVar Double -> IO ()
printRandDist i d = randDist i d >>= TLIO.putStrLn . showHist . toHist Nothing

-- | a random distribution, i.e. a set of randomly generated floats, of cardinality n
-- e.g. randDist 1300 (exponential 16)
randDist :: (MonadIO m, Ord a) => Int -> RVar a -> m (S.Set a)
randDist i = liftIO . replicateM' i . fmap S.singleton . flip runRVar DevURandom -- DevURandom generates pseudorandom numbers when the entropy pool is dry

-- | Horizontal text display of a histogram
-- ideally this will return an OpenGL graphic of a histogram. showHist isn't much more useful or easy to write for curses.
-- TODO: scale width of histogram output to number of columns
-- TODO: change floor to round to n digits
showHist :: (Foldable t, RealFrac n, Show n) => t ((n, n), Int) -> T.Text
showHist xs = case getMax' $ foldMap (Max' . length . printRange . both floor . fst) xs of -- actually, the last element should be the longest, assuming all elements have the same number of decimal digits
    (fromIntegral -> maxInXS) -> T.unlines $ foldMap (\((T.pack &&& fromIntegral . length) . printRange . both floor -> (t, l), fromIntegral -> numInBin) -> [t <> (T.take (maxInXS - l) (T.repeat ' ')) <> T.cons ' ' (T.take numInBin (T.repeat '◼'))]) xs
    where
        printRange (a, b) = show a ++ '~':(show b)

showHist' :: (Foldable t, Show a) => t (a, Int) -> T.Text
showHist' xs = case getMax' $ foldMap (Max' . length . show . fst) xs of -- actually, the last element should be the longest, assuming all elements have the same number of decimal digits
    (fromIntegral -> maxInXS) -> T.unlines $ foldMap (\((T.pack &&& fromIntegral . length) . show -> (t, l), fromIntegral -> numInBin) -> [t <> (T.take (maxInXS - l) (T.repeat ' ')) <> T.cons ' ' (T.take numInBin (T.repeat '◼'))]) xs

-- number of bins is determined by variance of distribution if Nothing is passed; else one may manually specify the number of bins
-- number of bins may be fewer than you requested, if the top bins aren't filled
-- retuns a set of ranges and the number of values falling in those ranges
-- TODO: how to automatically determine bin size?
-- BUG: toHist _ <$> randDist 1 (exponential _) hangs, even though the RHS of <$> evaluates fine
toHist :: (RealFloat n, Foldable t) => Maybe Int -> t n -> S.Set ((n, n), Int)
toHist (fromMaybe 20 -> numBins) dist = case (uncurry (-) . (getMax' *** getMin') $ foldMap (Max' &&& Min') dist) / fromIntegral numBins of -- binSize :: n
    binSize -> S.map (first ((id &&& (+binSize)) . (*binSize) . fromIntegral)) $ foldMap (S.singleton . second S.size) $ partitionBy (floor . (/binSize)) dist -- partition/sort data into bins, then replace bin indicies with value ranges

-- replace with Data.Foldable.maximum :: (Foldable t, Ord a) => t a -> a?
-- it'd be cleaner, but I can do Min' and Max' together in one fold.
newtype Min' a = Min' {getMin' :: a}
newtype Max' a = Max' {getMax' :: a}

instance (Num a, Ord a) => Semigroup (Min' a) where Min' a <> Min' b = Min' (min a b)
instance (Num a, Ord a) => Monoid (Min' a) where
    mappend = (<>) -- needed for GHC <8.4
    mempty = Min' (fromInteger 0)

instance (Num a, Ord a) => Semigroup (Max' a) where Max' a <> Max' b = Max' (max a b)
instance (Num a, Ord a) => Monoid (Max' a) where
    mappend = (<>)
    mempty = Max' (fromInteger 0)

-- | histogram where bins are single values rather than ranges, i.e. we count occurrences of unique values
toHist' :: (Ord n, Foldable t) => t n -> S.Set (n, Int) -- I could change Ord n to Eq n, and S.Set (n, Int) to [(n, Int)]
toHist' = S.map unOrderBy . foldr (\a b -> maybe (S.insert (OrderBy (a, 1)) b) (flip S.insert b . OrderBy . second succ) $ setFindOrd (==a) b) S.empty
    where
        setFindOrd :: (a -> Bool) -> S.Set (OrderBy a b) -> Maybe (a, b)
        setFindOrd p = fmap unOrderBy . listToMaybe . S.toList . S.filter (p . fst . unOrderBy)

-- next steps: generalize: use partitionBy more generally, and implement a system for composing filters combinatorially, so that one can filter in a venn-diagram style, or sort-through data to whittle-down to what they're looking for (building a scraper incrementially. remember that a scraper is merely a thing that identifies a particular subset from a given set (the given set will likely have to satisfy some predicates that validate the sensibility of the scraper.)
