module Main where

import qualified Circleci

import           Protolude
import           System.Directory

main :: IO ()
main = do
  args :: [[Char]] <- getArgs
  repoDir <- case args of
    ["here"] -> getCurrentDirectory
    ["in",dir] -> pure dir
    _ -> panic ("usage: here or in <dir>" :: Text)

  Circleci.main repoDir
