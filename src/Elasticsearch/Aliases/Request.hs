{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}

module Elasticsearch.Aliases.Request
  ( Action(..)
  , AddAttributes(..)
  , RemoveAttributes(..)
  , encode
  ) where

import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Json (pattern (:->))

import qualified Json

-- | The action to be taken with the document.
data Action
  = Add AddAttributes
    -- ^ Add an index to an alias
  | Remove RemoveAttributes
    -- ^ Remove an index from an alias
  deriving (Show)

data AddAttributes = AddAttributes
  { alias :: !ShortText
  , index :: !ShortText
  , isWriteIndex :: !Bool
  } deriving (Show)

data RemoveAttributes = RemoveAttributes
  { alias :: !ShortText
  , index :: !ShortText
  } deriving (Show)

encode :: SmallArray Action -> Json.Value
encode !xs = Json.object1 ("actions" :-> Json.Array (fmap encodeAction xs))

encodeAction :: Action -> Json.Value
encodeAction = \case
  Add x -> encodeAdd x
  Remove x -> encodeRemove x

encodeAdd :: AddAttributes -> Json.Value
encodeAdd AddAttributes{alias,index,isWriteIndex} =
  Json.object1 $ "add" :-> Json.object3
    ("alias" :-> Json.String alias)
    ("index" :-> Json.String index)
    ("is_write_index" :-> (if isWriteIndex then Json.True else Json.False))

encodeRemove :: RemoveAttributes -> Json.Value
encodeRemove RemoveAttributes{alias,index} =
  Json.object1 $ "remove" :-> Json.object2
    ("alias" :-> Json.String alias)
    ("index" :-> Json.String index)
