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
propTrieOfKeyListOfInt16TrieAble :: [[Int16]] -> Bool
propTrieOfKeyListOfInt16TrieAble xs = L.all (\x -> (fromByteString . toByteString $ x) == x) xs

propTrieOfKeyListOfInt32TrieAble :: [[Int32]] -> Bool
propTrieOfKeyListOfInt32TrieAble xs = L.all (\x -> (fromByteString . toByteString $ x) == x) xs

propTrieOfKeyListOfInt64TrieAble :: [[Int64]] -> Bool
propTrieOfKeyListOfInt64TrieAble xs = L.all (\x -> (fromByteString . toByteString $ x) == x) xs


class TrieAble t => PropListSorted t where
  propTrieOfKeyListSorted :: [t] -> Bool
  propTrieOfKeyListSorted xss = compare sorted_xss xss' == EQ
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

propTrieOfKeyListOfInt16Sorted :: [[Int16]] -> Bool
propTrieOfKeyListOfInt16Sorted = propTrieOfKeyListSorted

propTrieOfKeyListOfInt32Sorted :: [[Int32]] -> Bool
propTrieOfKeyListOfInt32Sorted = propTrieOfKeyListSorted

propTrieOfKeyListOfInt64Sorted :: [[Int64]] -> Bool
propTrieOfKeyListOfInt64Sorted = propTrieOfKeyListSorted

propTrieOfKeyTextSorted :: [String] -> Bool
propTrieOfKeyTextSorted = propTrieOfKeyListSorted . L.map Tx.pack