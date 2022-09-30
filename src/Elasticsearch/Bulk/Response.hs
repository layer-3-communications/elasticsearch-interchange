{-# language ApplicativeDo #-}
{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language UnboxedTuples #-}

-- | Response to @/_bulk@ request.
module Elasticsearch.Bulk.Response
  ( -- * Types
    Response(..)
  , Action(..)
  , Item(..)
  , ConcurrencyControl(..)
  , Error(..)
  , Details(..)
    -- * Response Parser
  , parser
    -- * Example Data
    -- $example
  ) where

import Prelude hiding (id)

import Control.Monad ((>=>))
import Data.Foldable (find)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word64,Word16)
import Elasticsearch.Bulk.Request (Action(Update,Create,Delete,Index))
import Json (Member(Member))
import Json.Parser (Parser,MemberParser)
import Json.Context (Context(Key))

import qualified Data.Primitive as PM
import qualified Json as J
import qualified Json.Parser as P

data Response = Response
  { took :: !Word64
    -- ^ How many milliseconds did the operation take?
  , errors :: !Bool
    -- ^ Did anything go wrong?
  , items :: !(SmallArray Item)
    -- ^ Individual responses for each operation.
  } deriving (Show)

-- | The _type field is omitted because it is always _doc in Elasticsearch 7+.
-- Some other fields are omitted because they are only present when an operation
-- succeeds.
data Item = Item
  { action :: !Action
  , index :: !ShortText
    -- ^ @_index@
  , id :: !ShortText
    -- ^ @_id@
  , status :: !Word16
    -- ^ @status@
  , details :: !Details
    -- ^ No single field dictates the details.
  } deriving (Show)

-- | An item has different fields depending on whether the operation
-- was considered to have succeeded.
data Details 
  = Success !ConcurrencyControl
  | Failure !Error
  deriving (Show)

data ConcurrencyControl = ConcurrencyControl
  { version :: !Word64
    -- ^ @_version@
  , sequenceNumber :: !Word64
    -- ^ @_seq_no@
  } deriving (Show)

data Error = Error
  { type_ :: !ShortText
  , reason :: !ShortText
  } deriving (Show)

-- | Decode the JSON response to a bulk request.
parser :: J.Value -> Parser Response
parser v = do
  mbrs <- P.object v
  P.members
    ( do took <- P.key "took" (P.number >=> P.word64)
         errors <- P.key "errors" P.boolean
         items <- P.key "items" (P.array >=> P.smallArray itemParser)
         pure Response{took,errors,items}
    ) mbrs

itemParser :: J.Value -> Parser Item
itemParser v = do
  mbrsTop <- P.object v
  case PM.sizeofSmallArray mbrsTop of
    1 -> do
      let !(# Member{key=keyAction,value=valueAction} #) = PM.indexSmallArray## mbrsTop 0
      action <- case keyAction of
        "update" -> pure Update
        "create" -> pure Create
        "delete" -> pure Delete
        "index" -> pure Index
        _ -> P.fail "expected one of: update, create, delete, index"
      P.contextually (Key keyAction) $ do
        mbrsProps <- P.object valueAction
        case find (\Member{key=keyProp} -> keyProp == "error") mbrsProps of
          Nothing -> P.members -- item indicates a success
            ( do index <- P.key "_index" P.string
                 id <- P.key "_id" P.string
                 status <- P.key "status" (P.number >=> P.word16)
                 version <- P.key "_version" (P.number >=> P.word64)
                 sequenceNumber <- P.key "_seq_no" (P.number >=> P.word64)
                 pure Item
                   { action,index,id,status
                   , details=Success ConcurrencyControl{version,sequenceNumber}
                   }
            ) mbrsProps
          Just Member{value=errorValue} -> do -- item indicates a failure
            mbrsError <- P.object errorValue
            err <- P.contextually (Key "error") (P.members errorMemberParser mbrsError)
            P.members
              ( do index <- P.key "_index" P.string
                   id <- P.key "_id" P.string
                   status <- P.key "status" (P.number >=> P.word16)
                   pure Item
                     { action,index,id,status
                     , details=Failure err
                     }
              ) mbrsProps
    _ -> P.fail "expected object with single member"

errorMemberParser :: MemberParser Error
errorMemberParser = do
  type_ <- P.key "type" P.string
  reason <- P.key "reason" P.string
  pure Error{type_,reason}


-- $example
--
-- Example responses from Elasticsearch documentation with additional
-- commentary. This one does not include any detailed error messages:
--
-- > {
-- >   "took": 30,
-- >   "errors": false,
-- >   "items": [
-- >     {
-- >       "index": {
-- >         "_index": "test",
-- >         "_type": "_doc",
-- >         "_id": "1",
-- >         "_version": 1,
-- >         "result": "created",
-- >         "_shards": {
-- >           "total": 2,
-- >           "successful": 1,
-- >           "failed": 0
-- >         },
-- >         "status": 201,
-- >         "_seq_no" : 0,
-- >         "_primary_term": 1
-- >       }
-- >     },
-- >     {
-- >       "delete": {
-- >         "_index": "test",
-- >         "_type": "_doc",
-- >         "_id": "2",
-- >         "_version": 1,
-- >         "result": "not_found",
-- >         "_shards": {
-- >           "total": 2,
-- >           "successful": 1,
-- >           "failed": 0
-- >         },
-- >         "status": 404,
-- >         "_seq_no" : 1,
-- >         "_primary_term" : 2
-- >       }
-- >     }
-- >   ]
-- > }
--
-- This one does have detailed error messages in it:
--
-- > {
-- >   "took": 486,
-- >   "errors": true,
-- >   "items": [
-- >     {
-- >       "update": {
-- >         "_index": "index1",
-- >         "_type" : "_doc",
-- >         "_id": "5",
-- >         "status": 404,
-- >         "error": {
-- >           "type": "document_missing_exception",
-- >           "reason": "[_doc][5]: document missing",
-- >           "index_uuid": "aAsFqTI0Tc2W0LCWgPNrOA",
-- >           "shard": "0",
-- >           "index": "index1"
-- >         }
-- >       }
-- >     },
-- >     {
-- >       "update": {
-- >         "_index": "index1",
-- >         "_type" : "_doc",
-- >         "_id": "6",
-- >         "status": 404,
-- >         "error": {
-- >           "type": "document_missing_exception",
-- >           "reason": "[_doc][6]: document missing",
-- >           "index_uuid": "aAsFqTI0Tc2W0LCWgPNrOA",
-- >           "shard": "0",
-- >           "index": "index1"
-- >         }
-- >       }
-- >     },
-- >     {
-- >       "create": {
-- >         "_index": "index1",
-- >         "_type" : "_doc",
-- >         "_id": "7",
-- >         "_version": 1,
-- >         "result": "created",
-- >         "_shards": {
-- >           "total": 2,
-- >           "successful": 1,
-- >           "failed": 0
-- >         },
-- >         "_seq_no": 0,
-- >         "_primary_term": 1,
-- >         "status": 201
-- >       }
-- >     }
-- >   ]
-- > }
--
-- Even though the documentation shows an @index_uuid@ field in the
-- error details, Elasticsearch 7.10 does not always populate this field.
-- It is not terribly useful, so it is omitted from the 'Error' type.
