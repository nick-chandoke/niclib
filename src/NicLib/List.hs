{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Operations that I want to merge into the ListLike package, but haven't yet (before doing that I want ListLike to be in terms of type families rather than func deps, so that both char8 bytestring and word8 bytestring can both be listlike's)
module NicLib.List
( breakAtLast
, breakOn
, chunksOf
, commonPrefixes
, count
, insertPeriodically
, intercalate
, replace
, rmLeading
, rmTrailing
, split
, splitOn
) where

import Control.Applicative ((<|>))
import NicLib.NStdLib
import qualified Data.List as L
import qualified Data.ListLike as LL

-- | remove an element from head, if head is that element, for ListLike
rmTrailing :: (LL.ListLike list item, Eq item) => item -> list -> list
rmTrailing c t = if LL.null t || LL.last t /= c then t else LL.init t

-- | remove an element from end, if last is that element
rmLeading :: (LL.ListLike list item, Eq item) => item -> list -> list
rmLeading x xs = if LL.null xs || LL.head xs /= x then xs else LL.tail xs

-- | inserts an element after every nth index
insertPeriodically :: (LL.ListLike full item) => Int -> item -> full -> full
insertPeriodically n i = LL.tail . fst . LL.foldl' (\(b,c) a -> (b `LL.append` if c `mod` n == 0 then LL.cons i (LL.singleton a) else LL.singleton a, succ c)) (LL.empty, 0)

-- | break at last seperator, e.g. breakAtLast '/' "/root/file/system" --> ("/root/file/", "system"). Nothing if seperator is not in sequence.
breakAtLast :: (LL.ListLike full item, Eq item) => item -> full -> Maybe (full, full)
breakAtLast sep = bool' (const Nothing) (go LL.empty . LL.reverse) LL.null
    where
        go stack t = LL.uncons t >>= \(x,xs) -> if x == sep then Just (LL.reverse t, stack) else go (LL.cons x stack) xs

-- | Data.Text.breakOn generalized to ListLike's
breakOn :: (LL.ListLike full item, Eq item) => full -> full -> (full, full)
breakOn p s
    | LL.null p = error "Data.ListLike.breakOn: empty input"
    | otherwise = go (LL.empty, s)
    where
        go (acc, s) =
            let (preMatch, atMatch) = LL.span (/= LL.head p) s in
                if LL.null atMatch || p `LL.isPrefixOf` atMatch then -- should be a tad faster to use commonPrefixes here
                    (acc `LL.append` preMatch, atMatch)
                else
                    go (acc `LL.append` (preMatch `LL.snoc` LL.head atMatch), LL.tail atMatch) -- token not yet found. keep searching through list

-- | Data.Text.commonPrefixes generalized to ListLike's
commonPrefixes :: (LL.ListLike full item, Eq item) => full -> full -> Maybe (full, full, full)
commonPrefixes = go LL.empty where
    go common a b =
        let v = if LL.null common then Nothing else Just (common, a, b)
            w = do -- isNothing when either list runs-out, or heads of list don't equal
            (x,xs) <- LL.uncons a
            (y,ys) <- LL.uncons b
            if (x == y) then go (LL.snoc common x) xs ys else v
        in w <|> v -- if we leave-off (<|> v), then if a is a sublist of b or vice-versa, commonPrefixes will incorrectly return Nothing

-- | Data.Text.replace generalized to ListLike's
replace :: (LL.ListLike full item, Eq item) => full -> full -> full -> full
replace p r s
    | LL.null p = error "Data.ListLike.replace: empty input"
    | otherwise = go s
    where
        !plen = LL.length p
        go s = case breakOn p s of
            (a, b) | LL.null b -> LL.empty
                   | otherwise -> a `LL.append` r `LL.append` go (LL.drop plen b)


-- | Data.Text.split generalized to ListLike's
split :: (LL.ListLike full item) => (item -> Bool) -> full -> [full]
split p xs = case LL.break p xs of
    (a, b) ->
        let cont = split p (LL.tail b) in
        if      LL.null b then LL.singleton xs
        else if LL.null a then cont
        else    LL.cons a cont

-- | Data.Text.splitOn generalized to ListLike's
splitOn :: (LL.ListLike full item, Eq item) => full -> full -> [full]
splitOn p s
    | LL.null p = error "Data.ListLike.splitOn: empty input"
    | otherwise = go s
    where
        !plen = LL.length p
        go s = case breakOn p s of
            (a, b) ->
                let cont = splitOn p (LL.drop plen b) in
                if      LL.null b then LL.singleton s
                else if LL.null a then cont
                else    LL.cons a cont

-- | Data.Text.chunksOf generalized to ListLike's
chunksOf :: (LL.ListLike full item) => Int -> full -> [full]
chunksOf 0 = const []
chunksOf i = go
    where
        go s | LL.null s = []
             | otherwise = case LL.splitAt i s of
                 (a, b) -> a : go b

-- | Data.Text.count generalized to ListLike's
count :: (LL.ListLike full item, Eq item) => full -> full -> Int
count a | LL.null a = error "Data.ListLike.count: empty input"
        | otherwise = go 0
    where
        !alen = LL.length a
        go n s = case breakOn a s of
            (_, y) | LL.null y -> n
                   | otherwise -> go (succ n) (LL.drop alen y)

-- | Data.Text.intercalate generalized to ListLike's
intercalate :: (LL.ListLike full item) => full -> [full] -> full
intercalate ins = go
    where
        go xs = case L.uncons xs of
            Nothing -> LL.empty
            Just (x,ss) -> x `LL.append` ins `LL.append` go ss

-- Q: why isn't every ListLike a FoldableLL defaulted by uncons?
{- Possibly relevant functions (that don't have trivial implementations, like compareLength or toUpper) in Text yet to be added to ListLike, StringLike, or FoldableLL:
T'.breakOnAll -- why would anyone use this instead of splitOn?
T'.mapAccumL, T'.mapAccumR
T'.scanl, T'.scanl1, T'.scanr T'.scanr1

-- Operations that require traversing from the end:
T'.stripEnd
T'.breakOnEnd
T'.dropAround, T'.dropEnd, T'.takeEnd, T'.takeWhileEnd
T'.unsnoc
-}
