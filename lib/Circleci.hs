module Circleci where

import           Circleci.Types
import           System.Process
import           Data.Aeson                               (Value, eitherDecode)
import qualified Data.List                                as List
import           Data.Aeson.Encode.Pretty                 (encodePretty)
import qualified Data.ByteString.Lazy                     as BSL
import           Data.FileEmbed                           (makeRelativeToProject,
                                                           strToExp)
import qualified Data.List.NonEmpty                       as NEL
import           Network.HTTP.Simple
import           Protolude
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Environment                       (lookupEnv)

circleciUrl :: [Char] -> [Char] -> [Char]
circleciUrl token branch =
  let
    user = "symbiont-io"
    project = "symbiont-node"
  in
    -- You need to follow the symbiont-node project in circleci for this url to work
    "https://circleci.com/api/v1.1/project/github/"<>user<>"/"<>project<>"/tree/"<>branch
      <> "?circle-token=" <> token <> "&limit=100" -- &offset=

main :: [Char] -> IO ()
main repoDir = do

  let
    record :: CreateProcess
    record = proc "/usr/bin/git" ["branch"]
    updatedRecord = record {cwd = Just repoDir}
    
  output <- readCreateProcess updatedRecord ""
  let
    lines :: [[Char]]
    lines = List.lines output
    filteredLines = List.filter (\text -> isPrefixOf "*" text) lines
    branch = "develop"
  putStrLn (show filteredLines :: Text)
  -- fetch circleci token from env var
  maybeToken :: Maybe [Char] <- lookupEnv "CIRCLECI_API_TOKEN"
  let
    token = case maybeToken of
      Nothing -> panic ("token missing" :: Text)
      Just t  -> t

  let url = circleciUrl token branch

  -- fetch json from circleci
  request :: Request <- parseRequest url :: IO Request -- (MonadThrow m) => m Request
  let request' = setRequestHeader "Accept" ["application/json"] request
  response :: Response BSL.ByteString <- httpLbs request' :: IO (Response BSL.ByteString) -- MonadIO m => m (Response BSL.ByteString)

  let
    bs :: BSL.ByteString
    bs = getResponseBody response

  -- parse raw json in order to pretty print and dump to file
  let
    json :: Value
    json = case eitherDecode bs of
      Left error  -> panic (toS error)
      Right value -> value
    filepath = $(makeRelativeToProject "result.json" >>= strToExp)

  atomicWriteFile filepath $ encodePretty json

  -- parse raw json into a haskell data type to work with it in a type-safe way
  let
    builds :: [CircleciBuild]
    builds = case eitherDecode bs of
      Left error  -> panic (toS error)
      Right value -> value

    groupedBuilds :: [NonEmpty CircleciBuild]
    groupedBuilds = NEL.groupWith (workflow_id . workflows) builds

  let
    _ops :: [IO ()]
    _ops = do
      build <- groupedBuilds
      pure $ putStrLn (show build :: Text)

    showWorkflow jobs = do
      let hash' = vcs_revision $ NEL.head jobs
      putStrLn hash'

    ops' :: [IO ()]
    ops' = map showWorkflow groupedBuilds

    ops'' :: IO [()]
    ops'' = sequence ops'

  void ops''
