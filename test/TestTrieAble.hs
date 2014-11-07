{-# LANGUAGE PackageImports, BangPatterns, FlexibleInstances, TypeSynonymInstances #-}
module TestTrieAble where

import Data.Text (Text)
import qualified Data.Text as Tx
import "bytestring-trie" Data.Trie (Trie)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Int

import TrieAble.TrieAble as T
import TrieAble.Instances()

(.$) :: a -> (a -> b) -> b
(.$) = flip ($)

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex xs = zip xs [0..]

nubAndSort :: Ord a => [a] -> ([a], [a])
nubAndSort xs = (S.toList balSet, S.toAscList balSet)
        where
                balSet = L.foldl' (flip insert') S.empty xs
                insert' k !set = S.insert k set

-------------------------------------------------------------------------------------
  
propKeyListOfInt16TrieAble :: [[Int16]] -> Bool
propKeyListOfInt16TrieAble = propIsKeyTrieAble

propKeyListOfInt32TrieAble :: [[Int32]] -> Bool
propKeyListOfInt32TrieAble = propIsKeyTrieAble

propKeyListOfInt64TrieAble :: [[Int64]] -> Bool
propKeyListOfInt64TrieAble = propIsKeyTrieAble


class TrieAble t => PropListSorted t where
  propTrieOfKeyListSorted :: [t] -> Bool
  propTrieOfKeyListSorted xs = compare sorted_xs xs' == EQ
        where
                xs' = T.keys tr1
                tr1 = L.reverse unsorted_xs
                       .$ zipWithIndex
                       .$ L.foldl' (flip insert') T.empty

                insert' (k, v) !t = T.insert k v t
                (unsorted_xs, sorted_xs) = nubAndSort xs

instance PropListSorted [Int16]
instance PropListSorted [Int32]
instance PropListSorted [Int64]
instance PropListSorted Text

propTrieOfKeyListOfInt16Sorted :: [[Int16]] -> Bool
propTrieOfKeyListOfInt16Sorted = propTrieOfKeyListSorted

propTrieOfKeyListOfInt32Sorted :: [[Int32]] -> Bool
propTrieOfKeyListOfInt32Sorted = propTrieOfKeyListSorted

propTrieOfKeyListOfInt64Sorted :: [[Int64]] -> Bool
propTrieOfKeyListOfInt64Sorted = propTrieOfKeyListSorted

propTrieOfKeyTextSorted :: [String] -> Bool
propTrieOfKeyTextSorted = propTrieOfKeyListSorted . L.map Tx.pack