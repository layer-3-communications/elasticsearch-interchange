{-# language ApplicativeDo #-}
{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language UnboxedTuples #-}

-- | Response
module Elasticsearch.Search.Response
  ( -- * Types
    Response(..)
  , Hits(..)
  , Hit(..)
  , Total(..)
    -- * Response Parser
  , parser
    -- * Example Data
    -- $example
  ) where

import Prelude hiding (id)

import Control.Monad ((>=>))
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word64)
import Json.Parser (Parser,MemberParser)

import qualified Json as J
import qualified Json.Parser as P

-- | A response from a search.
--
-- One strange thing about the organization of the response is that there
-- is a @hits@ field with another @hits@ field inside of it. This is how
-- elasticsearch presents this information, and that unusual structure is
-- simply mirrored by these types.
data Response = Response
  { took :: !Word64
    -- ^ How many milliseconds did the operation take?
  , hits :: !Hits
    -- ^ A hits object
  } deriving (Show)

data Hits = Hits
  { total :: !Total
    -- ^ How many milliseconds did the operation take?
  , hits :: !(SmallArray Hit)
    -- ^ Array of hits 
  } deriving (Show)

data Hit = Hit
  { index :: !ShortText
  , id :: !ShortText
  , source :: !J.Value
  } deriving (Show)

data Total = Total
  { value :: !Word64
  , relation :: !ShortText
  } deriving (Show)

-- | Decode the JSON response to a bulk request.
parser :: J.Value -> Parser Response
parser v = do
  mbrs <- P.object v
  P.members
    ( do took <- P.key "took" (P.number >=> P.word64)
         hits <- P.key "hits" (P.object >=> P.members hitsParser)
         pure Response{took,hits}
    ) mbrs

hitsParser :: MemberParser Hits
hitsParser = do
  total <- P.key "total" (P.object >=> P.members totalParser)
  hits <- P.key "hits"
    (P.array >=> P.smallArray (P.object >=> P.members hitParser))
  pure Hits{total,hits}

totalParser :: MemberParser Total
totalParser = do
  value <- P.key "value" (P.number >=> P.word64)
  relation <- P.key "relation" P.string
  pure Total{value,relation}

hitParser :: MemberParser Hit
hitParser = do
  index <- P.key "_index" P.string
  id <- P.key "_id" P.string
  source <- P.key "_source" pure
  pure Hit{index,id,source}

-- $example
--
-- Example response from Elasticsearch documentation:
--
-- {
--   "took": 5,
--   "timed_out": false,
--   "_shards": {
--     "total": 1,
--     "successful": 1,
--     "skipped": 0,
--     "failed": 0
--   },
--   "hits": {
--     "total": {
--       "value": 20,
--       "relation": "eq"
--     },
--     "max_score": 1.3862942,
--     "hits": [
--       {
--         "_index": "my-index-000001",
--         "_type" : "_doc",
--         "_id": "0",
--         "_score": 1.3862942,
--         "_source": {
--           "@timestamp": "2099-11-15T14:12:12",
--           "http": {
--             "request": {
--               "method": "get"
--             },
--             "response": {
--               "status_code": 200,
--               "bytes": 1070000
--             },
--             "version": "1.1"
--           },
--           "source": {
--             "ip": "127.0.0.1"
--           },
--           "message": "GET /search HTTP/1.1 200 1070000",
--           "user": {
--             "id": "kimchy"
--           }
--         }
--       }
--     ]
--   }
-- }
