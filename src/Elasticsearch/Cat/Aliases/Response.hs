{-# language ApplicativeDo #-}
{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language UnboxedTuples #-}

-- | Response to @/_cat/aliases@ request.
module Elasticsearch.Cat.Aliases.Response
  ( Response(..)
  , Alias(..)
    -- * Response Parser
  , parser
  ) where

import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Json.Parser (Parser)

import qualified Json as J
import qualified Json.Parser as P

newtype Response = Response
  { indices :: SmallArray Alias
  } deriving (Show)

data Alias = Alias
  { alias :: !ShortText
  , index :: !ShortText
  } deriving (Show)

parser :: J.Value -> Parser Response
parser v = fmap Response (P.array v >>= P.smallArray aliasParser)

aliasParser :: J.Value -> Parser Alias
aliasParser v = do
  mbrs <- P.object v
  P.members
    ( do alias <- P.key "alias" P.string
         index <- P.key "index" P.string
         pure Alias{alias,index}
    ) mbrs
