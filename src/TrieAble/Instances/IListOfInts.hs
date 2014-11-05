{-# LANGUAGE FlexibleInstances #-}
{-|
ListOfInts / byteString conversions to use in prefix trees (Tries)

Since negative Integers have binary representations higher as Word than positive ones
I add a posNegOrder prefix to the Big endian word8 rep. to restore negatives position
-}
module TrieAble.Instances.IListOfInts(
  propListOfInt32TrieAble,
  propListOfInt16TrieAble,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, int32BE, int16BE, word8, toLazyByteString)
import Data.ByteString.Lazy as LBS (toStrict, fromStrict)
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Bits
import Data.Word
import Data.Int
import Control.Exception (assert)
import Control.Applicative (liftA2)
import Data.Monoid

import TrieAble.TrieAble

(.$) :: a -> (a -> b) -> b
(.$) = flip ($)

instance TrieAble [Int32] where
  toByteString = toStrict . toLazyByteString . F.foldMap combine32
  fromByteString = L.map (fromIntegral . bs2Word32 . drop 1) . chunksOf 5 . BS.unpack

instance TrieAble [Int16] where
  toByteString = toStrict . toLazyByteString . F.foldMap combine16
  fromByteString = L.map (fromIntegral . bs2Word16 . drop 1) . chunksOf 3 . BS.unpack

chunksOf :: Int -> [Word8] -> [[Word8]]  
chunksOf n xs = assert (length xs `mod` n == 0) .  L.unfoldr (chunk n) $ xs
  where
    chunk n w8l = if L.null firsts
                 then Nothing
                 else Just (firsts, rest)
        where
          (firsts, rest) = splitAt n w8l

posNegOrder :: (Num a, Ord a, Integral a) => a -> Word8
posNegOrder = fromIntegral . (\x -> if x >= 0 then 1 else 0)

combine32 :: Int32 -> Builder
combine32 = liftA2 (mappend) (word8 . posNegOrder) int32BE

combine16 :: Int16 -> Builder
combine16 = liftA2 (mappend) (word8 . posNegOrder) int16BE


bs2Word32 :: [Word8] -> Word32
bs2Word32 [t,h,m,l] = shiftLFrom t 24 .|. shiftLFrom h 16 .|. shiftLFrom m 8 .|. fromIntegral l
  where
    shiftLFrom = shiftL . fromIntegral
          
bs2Word16 :: [Word8] -> Word16
bs2Word16 [m,l] = shiftLFrom m 8 .|. fromIntegral l
  where
    shiftLFrom = shiftL . fromIntegral

-------------------------------------------------------------------------------

-- Invariants


propListOfInt32TrieAble :: [[Int32]] -> Bool
propListOfInt32TrieAble xs = L.all (\x -> (fromByteString . toByteString $ x) == x) xs


propListOfInt16TrieAble :: [[Int16]] -> Bool
propListOfInt16TrieAble xs = L.all (\x -> (fromByteString . toByteString $ x) == x) xs