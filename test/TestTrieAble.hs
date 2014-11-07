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
propListOfInt16TrieAble :: [[Int16]] -> Bool
propListOfInt16TrieAble xs = L.all (\x -> (fromByteString . toByteString $ x) == x) xs

propListOfInt32TrieAble :: [[Int32]] -> Bool
propListOfInt32TrieAble xs = L.all (\x -> (fromByteString . toByteString $ x) == x) xs

propListOfInt64TrieAble :: [[Int64]] -> Bool
propListOfInt64TrieAble xs = L.all (\x -> (fromByteString . toByteString $ x) == x) xs


class TrieAble t => PropListSorted t where
  propListSorted :: [t] -> Bool
  propListSorted xss = compare sorted_xss xss' == EQ
        where
                xss' = T.keys tr1
                tr1 = L.reverse unsorted_xss
                       .$ zipWithIndex
                       .$ L.foldl' (flip insert') T.empty

                insert' (k, v) !t = T.insert k v t
                (unsorted_xss, sorted_xss) = nubAndSort xss

instance PropListSorted [Int16]
instance PropListSorted [Int32]
instance PropListSorted [Int64]
instance PropListSorted Text

propListOfInt16Sorted :: [[Int16]] -> Bool
propListOfInt16Sorted = propListSorted

propListOfInt32Sorted :: [[Int32]] -> Bool
propListOfInt32Sorted = propListSorted

propListOfInt64Sorted :: [[Int64]] -> Bool
propListOfInt64Sorted = propListSorted

propKeyTextSorted :: [String] -> Bool
propKeyTextSorted = propListSorted . L.map Tx.pack