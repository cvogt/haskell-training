module Main where

import qualified Circleci

import           Protolude
import           System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["list-here"] -> Circleci.list =<< getCurrentDirectory
    ["list-in",repo] -> Circleci.list repo
    ["trigger-here"] -> Circleci.trigger =<< getCurrentDirectory
    ["trigger-in", repo] -> Circleci.trigger repo
    ["list-previous"] -> Circleci.listResults
    _ -> putStrLn ("invalid usage" :: Text)
