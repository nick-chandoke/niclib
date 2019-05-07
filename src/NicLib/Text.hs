{-# OPTIONS_GHC -Wno-type-defaults #-}
module NicLib.Text
( abbreviate
, thousandComma
, quote
, indent
) where

import RIO hiding (unlines, replicate, lines, take, length)
import RIO.Text (Text, takeEnd, length)
import Data.Sequences -- mono-traversable
import Data.MonoTraversable (Element)

-- | Enclose in quotes
quote :: (SemiSequence seq, Element seq ~ Char) => seq -> seq
quote = cons '\"' . flip snoc '\"'

-- hack to make indent work with both Word8 and Char ByteString
class StrEmpty a where strEmpty :: a
instance StrEmpty Char where strEmpty = ' '
instance StrEmpty Word8 where strEmpty = 32

-- | Indent a given number of spaces. works for multiline strings too.
indent :: (Textual seq, StrEmpty (Element seq)) => Index seq -> seq -> seq
indent i = unlines . map (<> (replicate i strEmpty)) . lines

-- | Trims text to given length by removing characters from the middle; replaces removed text with an ellipsis (...)
--
-- This function uses 'Text.takeEnd' (which is optimized; using @LL.reverse . take n . LL.reverse@ would be much slower,) so it's defined only for Text's. It will be generalized to non-Texts when I push that commit of ListLike.
abbreviate :: Int -> Text -> Text
abbreviate n t =
    let tlen = length t
        numTake = floor ((fromIntegral (n - 3)) / 2)
    in if tlen <= n then t else (take numTake t) <> "..." <> (takeEnd (numTake + 1) t)

-- | Puts commas every 3 digits from the right
thousandComma :: (Foldable t, SemiSequence seq, Monoid seq, Element seq ~ Char) => t Char -> seq
thousandComma = fst . foldr (\a (b,c) -> (if c `mod` 3 == 0 && c /= 0 then cons a (cons ',' b) else cons a b, c + 1)) (mempty, 0)
