{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, BangPatterns #-}
{-|
ListOfInts / byteString conversions to use in prefix trees (Tries)

Since negative Integers have binary representations higher as Word than positive ones
I flip the sign bit in wordX rep. to restore non-negatives precedence
-}
module TrieAble.Instances.IListOfInts(
-- export instances only
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, word8, word16BE, word32BE, word64BE, toLazyByteString)
import Data.ByteString.Lazy as LBS (toStrict, fromStrict)
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Bits
import Data.Word
import Data.Int
import Control.Exception (assert)
import Data.Monoid


import TrieAble.TrieAble as T

(.$) :: a -> (a -> b) -> b
(.$) = flip ($)

instance TrieAble [Int16] where
  toByteString = toStrict . toLazyByteString . F.foldMap combine16
  fromByteString = L.map (fromIntegral . flipSignBit16 . bigEndianBs2WordX) . chunksOf 2 . BS.unpack

instance TrieAble [Int32] where
  toByteString = toStrict . toLazyByteString . F.foldMap combine32
  fromByteString = L.map (fromIntegral . flipSignBit32 . bigEndianBs2WordX) . chunksOf 4 . BS.unpack

instance TrieAble [Int64] where
  toByteString = toStrict . toLazyByteString . F.foldMap combine64
  fromByteString = L.map (fromIntegral . flipSignBit64 . bigEndianBs2WordX) . chunksOf 8 . BS.unpack

-------------------------------------------------------------------------------

chunksOf :: Int -> [Word8] -> [[Word8]]  
chunksOf n xs = assert (length xs `mod` n == 0) .  L.unfoldr (chunk n) $ xs
  where
    chunk n w8l = if L.null firsts
                 then Nothing
                 else Just (firsts, rest)
        where
          (firsts, rest) = splitAt n w8l

-----------------------------------------------------------------------          

flipSignBit16 :: Word16 -> Word16
flipSignBit16 = flip complementBit 15

flipSignBit32 :: Word32 -> Word32
flipSignBit32 = flip complementBit 31

flipSignBit64 :: Word64 -> Word64
flipSignBit64 = flip complementBit 63
          
combine16 :: Int16 -> Builder
combine16 x = (fromIntegral x :: Word16) .$ flipSignBit16 .$ word16BE 

combine32 :: Int32 -> Builder
combine32 x = (fromIntegral x :: Word32) .$ flipSignBit32 .$ word32BE

combine64 :: Int64 -> Builder
combine64 x = (fromIntegral x :: Word64) .$ flipSignBit64 .$ word64BE


-----------------------------------------------------------------------


class (Num t, Bits t) => BEByteString2WordX t where
  bigEndianBs2WordX :: [Word8] -> t      
  bigEndianBs2WordX ws = L.foldl' (.|.) 0 zipped
    where
      zipped = L.zipWith (shiftLFrom) (reverse ws) weights
      weights = L.map (*8) [0..]
      shiftLFrom = shiftL . fromIntegral
        
instance BEByteString2WordX Word16
instance BEByteString2WordX Word32
instance BEByteString2WordX Word64



                