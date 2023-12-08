{-# language ApplicativeDo #-}
{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language UnboxedTuples #-}

-- | Response to @/_cat/indices@ request. Make sure to set these query string
-- parameters: @format=json&bytes=b@.
module Elasticsearch.Cat.Indices.Response
  ( Response(..)
  , Index(..)
  , Health(..)
  , Status(..)
    -- * Response Parser
  , parser
  ) where

import Control.Monad ((>=>))
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word64)
import Elasticsearch.Internal (generousW64)
import Json.Parser (Parser)

import qualified Json as J
import qualified Json.Parser as P

newtype Response = Response
  { indices :: SmallArray Index
  } deriving (Show)

data Index = Index
  { health :: !Health
    -- ^ Health: green, yellow, red (@health@)
  , status :: !Status
    -- ^ Status: open or closed (@status@)
  , index :: !ShortText
    -- ^ The name of the index (@index@)
  , docsCount :: !Word64
    -- ^ Total number of documents (@docs.count@)
  , docsDeleted :: !Word64
    -- ^ Number of deleted documents (@docs.deleted@)
  } deriving (Show)

data Health = Green | Yellow | Red
  deriving (Show)

data Status = Open | Closed
  deriving (Show)

parser :: J.Value -> Parser Response
parser v = fmap Response (P.array v >>= P.smallArray indexParser)

indexParser :: J.Value -> Parser Index
indexParser v = do
  mbrs <- P.object v
  P.members
    ( do health <- P.key "health" (P.string >=> healthParser)
         status <- P.key "status" (P.string >=> statusParser)
         index <- P.key "index" P.string
         docsCount <- P.key "docs.count" generousW64
         docsDeleted <- P.key "docs.deleted" generousW64
         pure Index{health,status,index,docsCount,docsDeleted}
    ) mbrs

healthParser :: ShortText -> Parser Health
healthParser = \case
  "red" -> pure Red
  "yellow" -> pure Yellow
  "green" -> pure Green
  _ -> P.fail "expected one of: red, yellow, green"

statusParser :: ShortText -> Parser Status
statusParser = \case
  "open" -> pure Open
  "closed" -> pure Closed
  _ -> P.fail "expected one of: open, closed"
