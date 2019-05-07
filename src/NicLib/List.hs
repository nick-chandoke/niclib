{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | 'Text' operations generalized to 'IsSequence's.
module NicLib.List
( breakAtLast
, breakAtLastP
, replaceAll
, breakOn
, chunksOf
, commonPrefixes
, count
, insertPeriodically
, rmLeading
, rmTrailing
) where

import RIO hiding (drop, span, break)
import Data.Traversable (fmapDefault)
import Control.Applicative ((<|>))
import NicLib.NStdLib
import Data.Sequences (IsSequence, Index, initEx, tailEx, cons, singleton, break, uncons, snoc, span, splitAt, drop, isPrefixOf)
import Data.MonoTraversable (Element, onull, olength, headEx, lastEx)

-- | Replace all occurences of old with new. Using Traversable because we're replacing elements "in-place"; we're preserving structure and type.
--
-- cf. Foldable, which does not necessarily preserve structure.
replaceAll :: (Eq b, Traversable t) => b -> b -> t b -> t b
replaceAll old new = fmapDefault (bool' id (const new) (==old))

-- | Remove an element from head, if head is that element
rmTrailing :: (IsSequence seq, e ~ Element seq, Eq e) => e -> seq -> seq
rmTrailing c t = if onull t || lastEx t /= c then t else initEx t

-- | Remove an element from end, if last is that element
rmLeading :: (IsSequence seq, e ~ Element seq, Eq e) => e -> seq -> seq
rmLeading x xs = if onull xs || headEx xs /= x then xs else tailEx xs

-- | Inserts an element after every nth index
insertPeriodically :: (IsSequence seq, e ~ Element seq, Foldable t, Integral i) => i -> e -> t e -> seq
insertPeriodically n i = tailEx . fst . foldl' (\(b,c) a -> (b <> if c `mod` n == 0 then cons i (singleton a) else singleton a, c + 1)) (mempty, 0)

-- | Break at last separator.
--
-- >>> breakAtLast '/' "/root/file/system"
-- ("/root/file/", "system")
--
-- >>> breakAtLast '/' "./rel/file/path"
-- ("./rel/file/", "path")
--
-- >>> breakAtLast '.' "file.ext"
-- ("file.", "ext")
--
-- (By the way, to replace a filepath's extension, use 'NicLib.FileSystem.withExtension')
-- 
-- You may use @\case ("", _) -> ⋯@ to match against "there's no separator:"
--
-- >>> breakAtLast ';' "noseparator"
-- ("", "noseparator")
--
-- >>> breakAtLast '/' "dirpath/"
-- ("dirpath/", "")
--
-- Passing an empty list, regardless of separator, returns @mempty@:
--
-- >>> breakAtLast undefined ""
-- ("", "")
breakAtLast :: (IsSequence seq, e ~ Element seq, Eq e) => e -> seq -> (seq, seq)
breakAtLast sep = breakAtLastP (==sep)

-- TODO: can probably do faster via difference lists
-- | 'breakAtLast' generalized in the predicate
breakAtLastP :: IsSequence seq => (Element seq -> Bool) -> seq -> (seq, seq)
breakAtLastP p = go . (mempty,)
    where
        go z@(x, y) = case break p y of
            (a, b) ->
                case uncons b of
                    Nothing -> z
                    Just (bh, bs) -> go (x <> snoc a bh, bs)

-- | Generalized from 'Data.Text.breakOn'
breakOn :: (IsSequence seq, Eq (Element seq)) => seq -> seq -> (seq, seq)
breakOn p s
    | onull p = error "NicLib.List.breakOn: empty input"
    | otherwise = go (mempty, s)
    where
        go (acc, s) =
            let (preMatch, atMatch) = span (/= headEx p) s in
                if onull atMatch || p `isPrefixOf` atMatch then -- should be a tad faster to use commonPrefixes here
                    (acc <> preMatch, atMatch)
                else
                    go (acc <> (preMatch `snoc` headEx atMatch), tailEx atMatch) -- token not yet found. keep searching through list

-- | Generalized from 'Data.Text.commonPrefixes'
commonPrefixes :: (IsSequence seq, Monoid seq, Eq (Element seq)) => seq -> seq -> Maybe (seq, seq, seq)
commonPrefixes = go mempty where
    go common a b =
        let v = if onull common then Nothing else Just (common, a, b)
            w = do -- isNothing when either list runs-out, or heads of list don't equal
                (x,xs) <- uncons a
                (y,ys) <- uncons b
                if (x == y) then go (snoc common x) xs ys else v
        in w <|> v -- if we leave-off (<|> v), then if a is a sublist of b or vice-versa, commonPrefixes will incorrectly return Nothing

-- | Generalized from 'Data.Text.chunksOf'
chunksOf :: IsSequence seq => Index seq -> seq -> [seq]
chunksOf 0 = const []
chunksOf i = go
    where
        go s | onull s = []
             | otherwise = case splitAt i s of
                 (a, b) -> a : go b

-- | Generalized from 'Data.Text.count'
count :: (IsSequence seq, Eq (Element seq), Num n, Index seq ~ Int) => seq -> seq -> n
count a | onull a = error "NicLib.List.count: empty input"
        | otherwise = go 0
    where
        go n s = case breakOn a s of
            (_, y) | onull y -> n
                   | otherwise -> go (n + 1) (drop (olength a) y)
