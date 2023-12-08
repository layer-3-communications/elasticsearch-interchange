{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Elasticsearch.Internal
  ( generousW64
  ) where

import Data.Word (Word64)

import qualified Json as J
import qualified Json.Parser as P
import qualified Data.Number.Scientific as SCI
import qualified Data.Bytes.Text.Latin1 as Latin1
import qualified Data.Bytes.Text.Utf8 as Utf8

generousW64 :: J.Value -> P.Parser Word64
generousW64 = \case
  J.Number sci -> case SCI.toWord64 sci of
    Nothing -> P.fail "number out of 64-bit unsigned range"
    Just w -> pure w
  J.String t -> case Latin1.decodeDecWord (Utf8.fromShortText t) of
    Nothing -> P.fail "string was not decimal-encoded number"
    Just w -> pure $! fromIntegral @Word @Word64 w
  _ -> P.fail "expected number or decimal-encoded string"
