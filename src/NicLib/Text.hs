{-# LANGUAGE MultiWayIf, LambdaCase, FlexibleContexts, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module NicLib.Text
( abbreviate
, thousandComma
, removeComments
, quote
, indent
--, columnize
--, columnizeM
--, zipComplete
) where
import NicLib.NStdLib
import qualified Data.Text as T'
import qualified Data.ListLike as LL
import Data.ListLike.String (StringLike)
import qualified Data.ListLike.String as LLS
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BS'
import Data.String (IsString)
import Data.Word

--import System.Console.Terminfo.Cursor
--import System.Console.Terminfo.Base
{- try this after making both C8 and W8 BS' type family instances of ListLike
class (IsString s, StringLike s, LL.ListLike s Char, Semigroup s) => Stringy s
instance Stringy T.Text
instance Stringy T'.Text
instance Stringy BS'.ByteString
instance Stringy BS.ByteString
instance Stringy String
-}

-- enclose in quotes
quote :: (LL.ListLike str Char) => str -> str
quote = LL.cons '\"' . flip LL.snoc '\"'

-- hack to make indent work with ByteString
class StrEmpty a where strEmpty :: a
instance StrEmpty Char where strEmpty = ' '
instance StrEmpty Word8 where strEmpty = 32

-- | indent a given number of spaces. works for multiline strings too.
indent :: (LL.StringLike str, StrEmpty b, LL.ListLike str b) => Int -> str -> str
indent i = LLS.unlines . map (LL.append (LL.replicate i strEmpty)) . LLS.lines

-- really, the future is not text in terminals, nor curses even; it's some cross-platform (probably OpenGL-based) freeform-but-templated (FBT) graphics framework that automatically orders data based on its views; it's like an automatic-layout curses, but with the ability to use 3D space, display arbitrary graphics - those from disk or generated on-the-fly. It will be able to accept arbitrary key accelerators, too.
-- in the meantime, perhaps I can use a semi-curses style display, by generating HTML on-the-fly, leveraging elinks for display.
-- of course, the holy grail of UX is a big button that reads "do the thing," and the thing is so done. Basically, using a computer (for computing) is a group of:
-- 1. filter
-- 2. modify
-- 3. rearrange in search of new patterns
-- 4. pull-in new data
-- This design would make a good ng-comp. web browser (that separates view from model.) The controller is, as always, photon REPL.

-- | trims text to given length by removing characters from the middle; replaces removed text with an ellipsis (...)
-- this function uses Text.takeEnd (which is optimized; using (LL.reverse . take n . LL.reverse) would be much slower,) so it's defined only for Text's.
abbreviate :: Int -> T'.Text -> T'.Text
abbreviate n t =
    let tlen = LL.length t
        numTake = floor ((fromIntegral (n - 3)) / 2)
    in if tlen <= n then t else (LL.take numTake t) <> "..." <> (T'.takeEnd (numTake + 1) t)

-- | puts commas every 3 digits from the right
thousandComma :: (LL.ListLike str Char) => str -> str
thousandComma = fst . LL.foldr (\a (b,c) -> (if c `mod` 3 == 0 && c /= 0 then LL.cons a (LL.cons ',' b) else LL.cons a b, succ c)) (LL.empty, 0)

-- | removes {- comments -}
-- if you know how to make a parser, please feel free to improve this method
-- really, even now, it's silly to keep the previous character in fold state, since I'm using T'.init and T'.snoc so liberally anyway; I could almost certainly do better with T'.foldr
removeComments :: T'.Text -> T'.Text
removeComments t = case T'.uncons t of
    Nothing -> T'.empty
    Just (th,_) -> (\(a,_,_) -> a) . flip (flip T'.foldl ("", th, 0)) t $ \(acc, pc, n) c ->
        if | n > 0 && pc == '-' && c == '}' -> (acc, c, n - 1) -- parsing comment closes is only relevant if positive n. btw, consider these booleans carefully before thinking that you may re-express it better
           | pc == '{' && c == '-' -> (T'.init acc, c, n + 1)
           | otherwise -> (if n > 0 then acc else T'.snoc acc c, c, n)

{-
-- | columnize by pulling the COLUMNS environment variable, then calling columnize
columnizeM :: (MonadIO m) => ExceptT String m a
columnizeM = (\case Just cols -> printWithCols cols; _ -> normalPrintFunc) (setupTermFromEnv >>= flip getCapability termColumns) -- TODO: continue coding here

columnize :: (LL.ListLike full1 (LL.ListLike full2 item)) => Int ->
columnize numCols = 

-- functions related to columnizing output:
zipComplete :: (LL.ListLike a_full a_item, LL.ListLike b_full b_item, LL.ListLike c_full (Maybe a_item, Maybe b_item)) => a_full -> b_full -> c_full
zipComplete a b = case LL.uncons a of
    Nothing -> case LL.uncons b of
        Nothing -> LL.empty
        Just (b:bs) -> LL.zip (LL.repeat Nothing) (LL.map Just bs)
    Just (a:as) -> case LL.uncons b of
        Nothing -> LL.zip (LL.map Just as) (LL.repeat Nothing)
        Just (b:bs) -> zipComplete as bs `LL.append` LL.singleton (Just a, Just b)

mlength :: (LL.ListLike full item) => Maybe full -> Int
mlength = maybe 0 LL.length

            let zippedOutput = zipComplete (S.toList leftOutput) (S.toList rightOutput) -- TODO: general columnize function should fold through a series of columns, computing the max length for each, breaking early if the sum of the max lengths is greater than COLUMNS,...and put a separator string in between
            -- Data.Foldable.maximum :: (Foldable t, Ord a) => t a -> a
                (longestLeft, longestTotal) = foldl' (\(ll, lt) (mlength -> l1, mlength -> l2) -> (max l1 ll, max (l1 + l2) lt)) (0,0) zippedOutput
            in if longestTotal + 3 <= cols then -- +3 b/c " | " is separator string
                mapM_ (\(ms1, ms2) -> putStrLn $ maybe (replicate longestLeft ' ') (\s1 -> s1 <> replicate (longestLeft - length s1) ' ') ms1 <> " | " <> fromMaybe mempty ms2) zippedOutput
            else
                printSequentially
-}
-- TODO: consider the following code, and how to write it in terms of NicLib.Parser, &cf.w/b':' &c in original URL parser. Consider too that fmap's are nested here, and it's not as elegant as nesting scrapers
{-
s0 <- S.fromList
      . fmap (T'.unpack . T'.takeWhile (/=':'))
      . filter ((<500)
      . (read :: String -> Int)
      . T'.unpack . (!!2)
      . T'.split (==':'))
      . T'.lines
      <$> TIO.readFile "/etc/passwd" -- TODO: this map/filter should be one foldMap; make a function to make foldMap's of map/reduce pretty
let s1 = s0 S.\\ S.singleton "root"
    s2 = s1 S.\\ S.fromList ["sync", "shutdown", "halt", "root"]
-}
