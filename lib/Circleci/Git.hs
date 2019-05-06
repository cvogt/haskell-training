module Circleci.Git where

import qualified Data.Text      as T
import           Protolude
import           System.Process as P

newtype BranchName = BranchName [Char] deriving newtype (Print)
newtype GithubUser = GithubUser [Char]
newtype GithubProject = GithubProject [Char]

getBranch :: FilePath -> IO BranchName
getBranch repo = do
  let
    proc' = (P.proc "git" ["branch"]){
      P.cwd = Just repo
    }
  [branch] <- catMaybes <$> (T.stripPrefix "* " <<$>> T.splitOn "\n" . toS <$> P.readCreateProcess proc' "")
  pure $ BranchName $ toS branch

githubMeta :: FilePath -> IO (GithubUser, GithubProject)
githubMeta repo = do
  let
    proc' = (P.proc "git" ["remote","-v"]){
      P.cwd = Just repo
    }
  res <-
    (T.splitOn "/" <<$>>)
    . (catMaybes <$>)
    . (T.stripSuffix ".git (push)" <<$>>)
    . (catMaybes <$>)
    . (T.stripPrefix "origin\tgit@github.com:" <<$>>)
    . (T.splitOn "\n" . toS <$>)
    $ P.readCreateProcess proc' ""
  case res of
    [[user,project]] -> pure (GithubUser $ toS user,GithubProject $ toS project)
    other -> panic $ "githubMeta match fail: " <> show other

