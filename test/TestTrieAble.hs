{-# LANGUAGE PackageImports, BangPatterns #-}
module TestTrieAble where

import Data.Text (Text)
import qualified Data.Text as Tx
import "bytestring-trie" Data.Trie (Trie)
import qualified Data.Set as S
import qualified Data.List as L

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

propSorted :: [String] -> Bool
propSorted xs = compare sorted_xs' (T.keys tr1)  == EQ
        where
                tr1 = L.reverse unsorted_xs'
                       .$ zipWithIndex
                       .$ L.foldl' (flip insert') T.empty
                       
                insert' (k, v) !t = T.insert k v t      
                (unsorted_xs', sorted_xs') = nubAndSort $ L.map Tx.pack xs
                