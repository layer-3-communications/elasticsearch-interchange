{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}

module Elasticsearch.Bulk.Request
  ( Operation(..)
  , Action(..)
  , encode
  , encodeSmile
  ) where

import Data.Bytes.Builder (Builder)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import GHC.Exts (Ptr(Ptr))

import qualified Data.Maybe.Unpacked.Text.Short as M
import qualified Data.Bytes.Builder as Builder
import qualified Json
import qualified Json.Smile

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
  } deriving (Show)

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
  deriving (Show)

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

-- | Encode with SMILE. This does not use backreferences.
encodeSmile :: SmallArray Operation -> Builder
{-# noinline encodeSmile #-}
encodeSmile !xs = foldMap encodeSmileOne xs

encodeSmileAction :: Action -> ShortText -> ShortText -> Builder
{-# inline encodeSmileAction #-}
encodeSmileAction x !indexName !docId =
  ( case x of
      Create ->
        -- Breakdown:
        -- [0-3]: four-byte prefix, smiley face
        -- [4]: object (0xfa)
        -- [5]: key, length 6
        -- [6-11]: "create"
        -- [12]: object (0xfa)
        -- [13]: key, length 6
        -- [14-19]: "_index"
        Builder.cstring (Ptr "\x3a\x29\x0a\x00\xfa\x85\x63\x72\x65\x61\x74\x65\xfa\x85\x5f\x69\x6e\x64\x65\x78"#)
      Index -> 
        -- Nearly the same as create but with the action key different
        Builder.cstring (Ptr "\x3a\x29\x0a\x00\xfa\x84\x69\x6e\x64\x65\x78\xfa\x85\x5f\x69\x6e\x64\x65\x78"#)
      _ -> errorWithoutStackTrace "Elasticsearch.Bulk.Request[encodeSmileAction]: write Update and Delete cases"
  )
  <>
  Json.Smile.encodeString indexName
  <>
  Builder.cstring (Ptr "\x82\x5f\x69\x64"#) -- the "_id" key
  <>
  Json.Smile.encodeString docId
  <>
  Builder.cstring (Ptr "\xfb\xfb\xff"#) -- close objects and stream separator 0xFF

encodeSmileActionWithoutDocId :: Action -> ShortText -> Builder
{-# inline encodeSmileActionWithoutDocId #-}
encodeSmileActionWithoutDocId x !indexName =
  ( case x of
      Create ->
        Builder.cstring (Ptr "\x3a\x29\x0a\x00\xfa\x85\x63\x72\x65\x61\x74\x65\xfa\x85\x5f\x69\x6e\x64\x65\x78"#)
      Index -> 
        -- Nearly the same as create but with the action key different
        Builder.cstring (Ptr "\x3a\x29\x0a\x00\xfa\x84\x69\x6e\x64\x65\x78\xfa\x85\x5f\x69\x6e\x64\x65\x78"#)
      _ -> errorWithoutStackTrace "Elasticsearch.Bulk.Request[encodeSmileAction]: write Update and Delete cases"
  )
  <>
  Json.Smile.encodeString indexName
  <>
  Builder.cstring (Ptr "\xfb\xfb\xff"#) -- close objects and stream separator 0xFF

encodeSmileOne :: Operation -> Builder
encodeSmileOne Operation{action,index,id_,document} =
     M.maybe
       (encodeSmileActionWithoutDocId action index)
       (encodeSmileAction action index)
       id_
  <> Json.Smile.encode document
  <> Builder.word8 0xFF
