module TrieAble.Instances.InstanceText(
-- export only instances
) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf16BE, decodeUtf16BE)

import TrieAble.TrieAble

instance TrieAble (Text) where
  toByteString = encodeUtf16BE
  fromByteString = decodeUtf16BE
