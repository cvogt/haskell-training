module Main where

--import qualified Circleci

import           Protolude
import qualified Data.ByteString.Lazy as BSL
import           System.Environment (lookupEnv)
import           Network.HTTP.Simple (parseRequest, httpLbs, Request, Response, getResponseBody)

circleciUrl :: [Char] -> [Char]
circleciUrl token =
  let
    user = "symbiont-io"
    project = "symbiont-node"
    branch = "develop"
  in
    -- You need to follow the symbiont-node project in circleci for this url to work
    "https://circleci.com/api/v1.1/project/github/"<>user<>"/"<>project<>"/tree/"<>branch
      <> "?circle-token=" <> token <> "&limit=100" -- &offset=

main :: IO ()
main = do
  -- fetch circleci token from env var
  maybeToken :: Maybe [Char] <- lookupEnv "CIRCLECI_API_TOKEN"
  let
    token = case maybeToken of
      Nothing -> panic ("token missing" :: Text)
      Just t -> t

  let url = circleciUrl token

  putStrLn ("hi" :: Text)

  -- fetch json from circleci
  request :: Request <- parseRequest url :: IO Request -- (MonadThrow m) => m Request
  response :: Response BSL.ByteString <- httpLbs request :: IO (Response BSL.ByteString) -- MonadIO m => m (Response BSL.ByteString)

  let bs = getResponseBody response :: BSL.ByteString
  putStrLn bs

  -- pretty print json
