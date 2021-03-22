{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}

module Elasticsearch.Bulk.Request
  ( Operation(..)
  , Action(..)
  , encode
  ) where

import Data.Bytes.Builder (Builder)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import GHC.Exts (Ptr(Ptr))

import qualified Data.Maybe.Unpacked.Text.Short as M
import qualified Data.Bytes.Builder as Builder
import qualified Json

-- | A single operation in a bulk request.
data Operation = Operation
  { action :: !Action
    -- ^ The action to take. Usually 'Create' or 'Index'.
  , index :: {-# UNPACK #-} !ShortText
    -- ^ Index name, required
  , id_ :: {-# UNPACK #-} !M.MaybeShortText
    -- ^ Document ID, optional but strongly recommended.
  , document :: !Json.Value
    -- ^ Document 
  }

-- | The action to be taken with the document.
data Action
  = Create
    -- ^ Create a new document. Fails if document with ID already exists.
  | Index
    -- ^ Create or update a document.
  | Delete
    -- ^ Delete a document. Note: Currently broken. Results in
    --   malformed request.
  | Update
    -- ^ Update a document. Fails if document with ID does not exist.

-- | Encode returns a builder, not a JSON value, because the body of
-- an Elasticsearch bulk requests is not JSON. Technically, it is
-- <http://ndjson.org ndjson>, but since there is no common type for
-- that, this module just converts it straight to a builder.
encode :: SmallArray Operation -> Builder
encode !xs = foldMap encodeOne xs

encodeOne :: Operation -> Builder
encodeOne Operation{action,index,id_,document} =
     encodePreamble action
  <> Builder.shortTextJsonString index
  <> M.maybe mempty
     (\t -> Builder.ascii7 ',' '"' '_' 'i' 'd' '"' ':'
         <> Builder.shortTextJsonString t
     ) id_
  <> Builder.ascii3 '}' '}' '\n'
  <> Json.encode document
  <> Builder.ascii '\n'

-- When we encode the first part of the operation, we are trying to
-- build something like this:
--   {"index":{"_index":"test","_id":"1"}}
--   {"index":{"_index":"test"}
--   {"create":{"_index":"test","_id":"1"}}
-- This function encodes just the beginning part. For example:
--   {"index":{"_index":
--   {"create":{"_index":
encodePreamble :: Action -> Builder
encodePreamble x = case x of
  Create -> Builder.cstring (Ptr "{\"create\":{\"_index\":"# )
  Index -> Builder.cstring (Ptr "{\"index\":{\"_index\":"# )
  Delete -> Builder.cstring (Ptr "{\"delete\":{\"_index\":"# )
  Update -> Builder.cstring (Ptr "{\"update\":{\"_index\":"# )
