{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.Golden (goldenVsString)
import Elasticsearch.Bulk.Request (Operation(Operation),Action(Index,Create))
import Text.Show.Pretty (ppShow)
import Json (pattern (:->))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder
import qualified Data.List as List
import qualified Data.Maybe.Unpacked.Text.Short as M
import qualified GHC.Exts as Exts
import qualified Elasticsearch.Bulk.Request as Bulk.Request
import qualified Elasticsearch.Bulk.Response as Bulk.Response
import qualified Elasticsearch.Search.Response as Search.Response
import qualified Elasticsearch.Cat.Indices.Response as Cat.Indices.Response
import qualified Elasticsearch.Cat.Aliases.Response as Cat.Aliases.Response
import qualified Elasticsearch.Aliases.Request as Aliases.Request
import qualified Json
import qualified Json.Errors as Errors
import qualified Json.Parser as Parser
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.Text.Short as TS

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Elasticsearch"
  [ testGroup "Search"
    [ testGroup "Response"
      [ goldenVsString "001" "samples/search/response/001/output.txt" $ do
          prepare "samples/search/response/001/input.json" Search.Response.parser
      ]
    ]
  , testGroup "CatAliases"
    [ testGroup "Response"
      [ goldenVsString "001" "samples/cat/aliases/response/001/output.txt" $ do
          prepare "samples/cat/aliases/response/001/input.json" Cat.Aliases.Response.parser
      ]
    ]
  , testGroup "CatIndices"
    [ testGroup "Response"
      [ goldenVsString "001" "samples/cat/indices/response/001/output.txt" $ do
          prepare "samples/cat/indices/response/001/input.json" Cat.Indices.Response.parser
      ]
    ]
  , testGroup "Aliases"
    [ testGroup "Request"
      [ goldenVsString "001" "samples/aliases/request/001/output.json"
          $ pure
          $ LBS.fromStrict
          $ Chunks.concatByteString
          $ Builder.run 64
          $ Json.encode
          $ Aliases.Request.encode
          $ Exts.fromList
            [ Aliases.Request.Add Aliases.Request.AddAttributes
              { alias = "person-2018"
              , index = "person-2018.09.28"
              , isWriteIndex = False
              }
            , Aliases.Request.Remove Aliases.Request.RemoveAttributes
              { alias = "person-2018"
              , index = "person-2018.09.27"
              }
            ]
      ]
    ]
  , testGroup "Bulk"
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
          prepare "samples/bulk/response/001/input.json" Bulk.Response.parser
      ]
    ]
  ]

prepare :: Show a
  => String -- path to json
  -> (Json.Value -> Parser.Parser a)
  -> IO LBC8.ByteString
prepare path parse = do
  c <- Bytes.readFile path
  case Json.decode c of
    Left _ -> fail "input file was not JSON"
    Right v -> do
      case Parser.run (parse v) of
        Left errs -> fail ("parse error at: " ++  TS.unpack (Errors.encode errs))
        Right response -> do
          let raw = ppShow response
          let clean = if List.isSuffixOf "\n" raw then raw else raw ++ "\n"
          pure (LBC8.pack clean)
