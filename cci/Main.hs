module Main where

--import qualified Circleci

import           Data.Aeson (eitherDecode, Value, FromJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import           Data.FileEmbed (makeRelativeToProject, strToExp)
import           Protolude
import           Network.HTTP.Simple
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Environment (lookupEnv)

circleciUrl :: [Char] -> [Char]
circleciUrl token =
  let
    user = "symbiont-io"
    project = "symbiont-node"
    branch = "develop"
  in
    -- You need to follow the symbiont-node project in circleci for this url to work
    "https://circleci.com/api/v1.1/project/github/"<>user<>"/"<>project<>"/tree/"<>branch
      <> "?circle-token=" <> token <> "&limit=1" -- &offset=

data CircleciBuild =
  CircleciBuild
    { status :: Text
    , queued_at :: Text
    }
  deriving (Generic, FromJSON, Show)

main :: IO ()
main = do
  -- fetch circleci token from env var
  maybeToken :: Maybe [Char] <- lookupEnv "CIRCLECI_API_TOKEN"
  let
    token = case maybeToken of
      Nothing -> panic ("token missing" :: Text)
      Just t -> t

  let url = circleciUrl token

  -- fetch json from circleci
  request :: Request <- parseRequest url :: IO Request -- (MonadThrow m) => m Request
  let request' = setRequestHeader "Accept" ["application/json"] request
  response :: Response BSL.ByteString <- httpLbs request' :: IO (Response BSL.ByteString) -- MonadIO m => m (Response BSL.ByteString)

  let
    bs :: BSL.ByteString
    bs = getResponseBody response
    json :: Value
    json = case eitherDecode bs of
      Left error -> panic (toS error)
      Right value -> value
    build :: [CircleciBuild]
    build = case eitherDecode bs of
      Left error -> panic (toS error)
      Right value -> value
    filepath = $(makeRelativeToProject "result.json" >>= strToExp)

  atomicWriteFile filepath $ encodePretty json

  -- pretty print json

  putStrLn $ (show build :: Text)

