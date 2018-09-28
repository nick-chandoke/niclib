{-# OPTIONS_GHC -Wno-type-defaults #-}
module NicLib.Text
( abbreviate
, thousandComma
, removeComments
, quote
, indent
, truncToNDigits
) where

import Data.Word
import qualified Data.ListLike as LL
import qualified Data.ListLike.String as LLS
import qualified Data.Text as T'

-- enclose in quotes
quote :: (LL.ListLike str Char) => str -> str
quote = LL.cons '\"' . flip LL.snoc '\"'

-- hack to make indent work with ByteString (again, shouldn't be necessary after moving ListLike to TypeFamilies rather than FuncDeps)
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
-- this function uses Text.takeEnd (which is optimized; using (LL.reverse . take n . LL.reverse) would be much slower,) so it's defined only for Text's. It will be generalized to non-Texts when I push that commit of ListLike.
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

-- | truncate a RealFloat's decimal to a given number of digits
truncToNDigits :: (Show a) => Int -> a -> String
truncToNDigits n = (\(a,b) -> a <> take (n + 1) b) . break (=='.') . show
