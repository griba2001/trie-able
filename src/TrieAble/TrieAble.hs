{-# LANGUAGE PackageImports, OverloadedStrings, NamedFieldPuns #-}
module TrieAble.TrieAble (
  TrieAble(..),
  empty, singleton,
  null, size,
  insert, delete,
  member, lookup,
  adjust, alterBy,
  mapBy, filterMap,
  fromList, toList, toListBy, keys,
  unionL, unionR, mergeBy,        
) where

import Prelude hiding (null, lookup)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import "bytestring-trie" Data.Trie (Trie)
import qualified "bytestring-trie" Data.Trie as T
import qualified Data.List as L

class (Ord k) => TrieAble k where
  toByteString :: k -> ByteString
  fromByteString :: ByteString -> k

--------------------------------------

-- convert pairs
mapPairKey :: TrieAble k => (k, v) -> (ByteString, v)
mapPairKey (k, v) = (toByteString k, v)

mapBSPairKey :: TrieAble k =>  (ByteString, v) -> (k, v)
mapBSPairKey (bs, v) = (fromByteString bs, v)

-- convert functions of key to functions of ByteString to be passed in high order functions like mapBy
convertFKey :: TrieAble k => (k -> a) -> ByteString -> a
convertFKey f bs = f k
        where k = fromByteString bs

--------------------------------------

empty :: Trie v
empty = T.empty

singleton :: TrieAble k => k -> v -> Trie v
singleton k v = T.singleton (toByteString k) v

null :: Trie v -> Bool
null = T.null

size :: Trie v -> Int
size = T.size

--------------------------------------

fromList :: TrieAble k => [(k, a)] -> Trie a
fromList = T.fromList . L.map mapPairKey

toList :: TrieAble k => Trie a -> [(k, a)]
toList = L.map mapBSPairKey . T.toList

--------------------------------------

insert :: TrieAble k =>  k -> a -> Trie a -> Trie a
insert k =  T.insert (toByteString k)

delete :: TrieAble k =>  k -> Trie a -> Trie a
delete k =  T.delete (toByteString k)

adjust :: TrieAble k => (a -> a) -> k -> Trie a -> Trie a
adjust f k = T.adjust f (toByteString k)

lookup :: TrieAble k =>  k -> Trie a -> Maybe a
lookup k = T.lookup (toByteString k)

member :: TrieAble k =>  k -> Trie a -> Bool
member k = T.member (toByteString k)

submap :: TrieAble k =>  k -> Trie a -> Trie a
submap k = T.submap (toByteString k)
--------------------------------------

unionL, unionR :: Trie a -> Trie a -> Trie a
unionL = T.unionL
unionR = T.unionR

mergeBy :: (a -> a -> Maybe a) -> Trie a -> Trie a -> Trie a
mergeBy = T.mergeBy

filterMap :: (a -> Maybe b) -> Trie a -> Trie b
filterMap = T.filterMap

--------------------------------------

mapBy :: TrieAble k => (k -> a -> Maybe b) -> Trie a -> Trie b
mapBy f = T.mapBy (convertFKey f)

alterBy :: TrieAble k => (k -> a -> Maybe a -> Maybe a) -> k -> a -> Trie a -> Trie a
alterBy f k = T.alterBy (convertFKey f) (toByteString k)

toListBy :: TrieAble k => (k -> a -> b) -> Trie a -> [b]
toListBy f = T.toListBy (convertFKey f)

keys :: TrieAble k => Trie a -> [k]
keys = L.map fromByteString . T.keys
