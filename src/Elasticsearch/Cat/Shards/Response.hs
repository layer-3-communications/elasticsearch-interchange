{-# language ApplicativeDo #-}
{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language UnboxedTuples #-}

-- | Response to @/_cat/shards@ request. Make sure to set these query string
-- parameters: @format=json&bytes=b@.
module Elasticsearch.Cat.Shards.Response
  ( Response(..)
  , Shard(..)
  , Type(..)
  , State(..)
    -- * Response Parser
  , parser
  ) where

import Control.Monad ((>=>))
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word32)
import Elasticsearch.Internal (generousW32)
import Json.Parser (Parser)

import qualified Json as J
import qualified Json.Parser as P

newtype Response = Response
  { shards :: SmallArray Shard
  } deriving (Show)

data Shard = Shard
  { index :: !ShortText
    -- ^ The name of the index (@index@)
  , shard :: !Word32
    -- ^ The shard number (@shard@)
  , type_ :: !Type
    -- ^ Shard type: primary or replica (@prirep@)
  , state :: !State
    -- ^ State: initializing, relocating, started, unassigned (@state@)
  , node :: !ShortText
    -- ^ Name of the node that hosts the shard (@node@)
  } deriving (Show)

data Type = Primary | Replica
  deriving (Show)

data State = Initializing | Relocating | Started | Unassigned
  deriving (Show)

parser :: J.Value -> Parser Response
parser v = fmap Response (P.array v >>= P.smallArray shardParser)

shardParser :: J.Value -> Parser Shard
shardParser v = do
  mbrs <- P.object v
  P.members
    ( do index <- P.key "index" P.string
         shard <- P.key "shard" generousW32
         type_ <- P.key "prirep" (P.string >=> typeParser)
         state <- P.key "state" (P.string >=> stateParser)
         node <- P.key "node" P.string
         pure Shard{index,shard,type_,state,node}
    ) mbrs

typeParser :: ShortText -> Parser Type
typeParser = \case
  "r" -> pure Replica
  "p" -> pure Primary
  _ -> P.fail "expected one of: primary, replica"

stateParser :: ShortText -> Parser State
stateParser = \case
  "STARTED" -> pure Started
  "INITIALIZING" -> pure Initializing
  "RELOCATING" -> pure Relocating
  "UNASSIGNED" -> pure Unassigned
  _ -> P.fail "expected one of: STARTED, INITIALIZING, RELOCATING, UNASSIGNED"

