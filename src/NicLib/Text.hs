{-# OPTIONS_GHC -Wno-type-defaults #-}
module NicLib.Text
( abbreviate
, thousandComma
, quote
, indent
) where

import RIO
import Prelude (succ)

import Data.Word
import qualified Data.ListLike as LL
import qualified Data.ListLike.String as LLS
import qualified RIO.Text as T'

-- Enclose in quotes
quote :: (LL.ListLike str Char) => str -> str
quote = LL.cons '\"' . flip LL.snoc '\"'

-- hack to make indent work with ByteString (again, shouldn't be necessary after moving ListLike to TypeFamilies rather than FuncDeps)
class StrEmpty a where strEmpty :: a
instance StrEmpty Char where strEmpty = ' '
instance StrEmpty Word8 where strEmpty = 32

-- | Indent a given number of spaces. works for multiline strings too.
indent :: (LL.StringLike str, StrEmpty b, LL.ListLike str b) => Int -> str -> str
indent i = LLS.unlines . map (LL.append (LL.replicate i strEmpty)) . LLS.lines

-- | Trims text to given length by removing characters from the middle; replaces removed text with an ellipsis (...)
--
-- This function uses 'Text.takeEnd' (which is optimized; using @LL.reverse . take n . LL.reverse@ would be much slower,) so it's defined only for Text's. It will be generalized to non-Texts when I push that commit of ListLike.
abbreviate :: Int -> T'.Text -> T'.Text
abbreviate n t =
    let tlen = LL.length t
        numTake = floor ((fromIntegral (n - 3)) / 2)
    in if tlen <= n then t else (LL.take numTake t) <> "..." <> (T'.takeEnd (numTake + 1) t)

-- | Puts commas every 3 digits from the right
thousandComma :: (LL.ListLike str Char) => str -> str
thousandComma = fst . LL.foldr (\a (b,c) -> (if c `mod` 3 == 0 && c /= 0 then LL.cons a (LL.cons ',' b) else LL.cons a b, succ c)) (LL.empty, 0)
