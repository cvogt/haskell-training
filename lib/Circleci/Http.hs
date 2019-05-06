module Circleci.Http where

import           Data.Aeson
import           Network.HTTP.Conduit
import           Protolude
import           System.AtomicWrite.Writer.ByteString

newtype Url = Url [Char] deriving newtype Print

httpsPostJson :: ToJSON a => Manager -> Url -> a -> IO ()
httpsPostJson manager (Url url) a = do
  req' <- parseRequest url
  let
    req = req' {
      method = "POST",
      secure = True,
      requestBody = RequestBodyLBS $ toS $ encode a,
      requestHeaders =
        ("Content-Type", "application/json") : requestHeaders req'
    }
  response <- httpLbs req manager
  putStrLn $ responseBody response

httpsGetJson :: Manager -> Url -> FilePath -> IO ()
httpsGetJson manager (Url url) resultFile = do
  putStrLn $ "downloading " <> url
  req' <- parseRequest url
  let
    req = req' {
      method = "GET",
      secure = True,
      requestHeaders =
        ("Accept:", "application/json") : requestHeaders req'
    }
  response <- httpLbs req manager
  atomicWriteFile resultFile $ toS $ responseBody response
