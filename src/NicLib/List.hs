-- | TODO: Merge with ListLike
module NicLib.List
( insertPeriodically
, breakAtLast
, split
) where
import NicLib.NStdLib
import qualified Data.ListLike as LL

-- | inserts an element after every nth index
insertPeriodically :: (LL.ListLike full item) => Int -> item -> full -> full
insertPeriodically n i = LL.tail . fst . LL.foldl' (\(b,c) a -> (b `LL.append` if c `mod` n == 0 then LL.cons i (LL.singleton a) else LL.singleton a, succ c)) (LL.empty, 0)

-- | break at last seperator, e.g. breakAtLast '/' "/root/file/system" --> ("/root/file/", "system"). Nothing if seperator is not in sequence.
breakAtLast :: (LL.ListLike full item, Eq item) => item -> full -> Maybe (full, full)
breakAtLast sep xs
    | LL.null xs = Nothing
    | otherwise = go LL.empty (LL.reverse xs)
    where
        go stack t = LL.uncons t >>= \(x,xs) -> if x == sep then Just (LL.reverse t, stack) else go (LL.cons x stack) xs

split :: (a -> Bool) -> [a] -> [[a]]
split p = filter (not . null) . uncurry (:) . foldr (\a (buf,xs) -> p a ? (a:buf, xs) ↔ ([], buf:xs)) mempty
