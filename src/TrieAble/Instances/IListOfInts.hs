{-# LANGUAGE FlexibleInstances #-}
module TrieAble.Instances.IListOfInts(
-- export only instances
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (int32BE, int16BE, toLazyByteString)
import Data.ByteString.Lazy as LBS (toStrict, fromStrict)
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Bits
import Data.Word
import Data.Int
import Control.Exception (assert)

import TrieAble.TrieAble

(.$) :: a -> (a -> b) -> b
(.$) = flip ($)

instance TrieAble [Int32] where
  toByteString = toStrict . toLazyByteString . F.foldMap int32BE
  fromByteString = L.map (fromIntegral . bs2Word32) . chunksOf 4 . BS.unpack

instance TrieAble [Int16] where
  toByteString = toStrict . toLazyByteString . F.foldMap int16BE
  fromByteString = L.map (fromIntegral . bs2Word16) . chunksOf 2 . BS.unpack

chunksOf :: Int -> [Word8] -> [[Word8]]  
chunksOf n xs = assert (length xs `mod` n == 0) .  L.unfoldr (chunk n) $ xs
  where
    chunk n w8l = if L.null firsts
                 then Nothing
                 else Just (firsts, rest)
        where
          (firsts, rest) = splitAt n w8l
  

bs2Word32 :: [Word8] -> Word32
bs2Word32 [t,h,m,l] = shiftLFrom t 24 .|. shiftLFrom h 16 .|. shiftLFrom m 8 .|. fromIntegral l
  where
    shiftLFrom x = shiftL (fromIntegral x)
          
bs2Word16 :: [Word8] -> Word16
bs2Word16 [m,l] = shiftLFrom m 8 .|. fromIntegral l
  where
    shiftLFrom x = shiftL (fromIntegral x)
