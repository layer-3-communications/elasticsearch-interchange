{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.Golden (goldenVsString)
import Elasticsearch.Bulk.Request (Operation(Operation),Action(Index,Create))
import Elasticsearch.Bulk.Response (Details,Error,ConcurrencyControl,Item,Response)
import Text.Show.Pretty (ppShow)
import Json (pattern (:->))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder
import qualified Data.Maybe.Unpacked.Text.Short as M
import qualified GHC.Exts as Exts
import qualified Elasticsearch.Bulk.Request as Bulk.Request
import qualified Elasticsearch.Bulk.Response as Bulk.Response
import qualified Json
import qualified Json.Path as Path
import qualified Json.Parser as Parser
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.Text.Short as TS

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Elasticsearch"
  [ testGroup "Bulk"
    [ testGroup "Request"
      [ goldenVsString "001" "samples/bulk/request/001/output.json"
          $ pure
          $ LBS.fromStrict
          $ Chunks.concatByteString
          $ Builder.run 64
          $ Bulk.Request.encode
          $ Exts.fromList
            [ Operation
              { action = Index
              , index = "person-2018.09.28"
              , id_ = M.just "5bc6d38e-8b30-11eb-8dcd-0242ac130003"
              , document = Json.emptyObject
              }
            , Operation
              { action = Create
              , index = "person-2018.09.29"
              , id_ = M.just "3cd512c8-8b31-11eb-8dcd-0242ac130003"
              , document = Json.Object
                  (Exts.fromList ["name" :-> Json.String "Xavier"])
              }
            ]
      ]
    , testGroup "Response"
      [ goldenVsString "001" "samples/bulk/response/001/output.txt" $ do
          c <- Bytes.readFile "samples/bulk/response/001/input.json"
          case Json.decode c of
            Left _ -> fail "input file was not JSON"
            Right v -> do
              case Parser.run (Bulk.Response.parser v) of
                Left path -> fail ("parse error at: " ++ foldMap (\p -> ',' : TS.unpack (Path.encode p)) (Parser.getMultipath path))
                Right response -> pure (LBC8.pack (ppShow response))
      ]
    ]
  ]

deriving instance Show Response
deriving instance Show Action
deriving instance Show Item
deriving instance Show ConcurrencyControl
deriving instance Show Error
deriving instance Show Details
