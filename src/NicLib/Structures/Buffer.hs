{-# OPTIONS_GHC -funbox-strict-fields -O2 -optc-O2 #-}
-- | Push elements in a fixed-size buffer; if buffer is full at time of element insertion, oldest element is pushed out from the buffer. I'd think there's a name for such a structure, and that someone else would have already implemented it. If you know about such things, please message me about it.
--
-- Intended to be imported qualified
--
-- Undefined behavior for Buffers of size greater than @maxBound :: Int@
--
-- Works via <https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Unboxed.html unboxed vectors>; Buffers aren't defined for general objects.
--
-- Should use 'IOUArray''s for greatest speed,...but it's nice to use Vectors for pure functions.... Maybe I'll generalize Buffer to both later. For now, Vector is used as a happy medium between IOUArray and [].
--
-- The following is an example using a buffer of ints:
--
-- @
-- import qualified NicLib.Structures.Buffer as Buf
-- foldM_ (\\b n -\> let b' = Buf.push b n in print b' *\> pure b') (Buf.create 6) ([12..20 :: Int])
-- @
--
-- A buffer is, in memory, a constant size. However, the get function trims the list to the number of pushed elements. Thus the size of the vector returned by get may be smaller than the size you initialized the buffer to.
module NicLib.Structures.Buffer
( Buffer -- do not export constructor
, get
, create
, push
) where

import RIO
import qualified Data.Vector as V
import Data.Vector.Unboxed (Unbox)

data Buffer a = Buffer
    !Int -- index
    !Int -- size
    !(V.Vector a) -- data
    deriving (Eq, Ord, Functor)
instance (Show a, Unbox a) => Show (Buffer a) where show = show . get

-- | Allocate memory for a buffer of given size
create :: Int -> Buffer a
create size = Buffer 0 size (V.replicate size undefined)

-- | Convert a buffer to a vector, whereby its elements may be viewed and manipulated
get :: Unbox a => Buffer a -> V.Vector a
get (Buffer i s b) =
    let i' = i `mod` s
        p = V.slice 0 i' b
    in if i < s then p else V.slice i' (s - i') b <> p

-- I'm uncomfortable that I'm returning a new buffer rather than the same buffer in the ST monad
-- see <http://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array-IO.html>
-- | Add an element to the buffer
push :: Unbox a => a -> Buffer a -> Buffer a
push e (Buffer i s b) = Buffer (i + 1) s (V.update b (V.singleton (i `mod` s, e))) -- I'm not clear on how to use V.modify, so I'm using V.update....
