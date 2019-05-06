module Circleci where

import           Circleci.Git
import           Circleci.Http
import           Circleci.Types

import           Data.Aeson
import           Data.FileEmbed
import qualified Data.Text                   as T
import           GHC.Exts                    (groupWith)
import           Network.HTTP.Client.Conduit (defaultManagerSettings)
import           Network.HTTP.Conduit        (Manager, newManager)
import           Protolude
import           System.Environment          (lookupEnv)

resultFile :: FilePath
resultFile = $(makeRelativeToProject "result.json" >>= strToExp) :: FilePath

init :: FilePath -> IO (Manager, CircleciApiToken, BranchName, Url)
init repo = do
  manager <- newManager defaultManagerSettings
  circleci_api_token <- maybe (panic "please set CIRCLECI_API_TOKEN env var") CircleciApiToken <$> lookupEnv "CIRCLECI_API_TOKEN"
  branch <- getBranch repo
  (GithubUser githubUser, GithubProject githubProject) <- githubMeta repo
  let base_project_url = Url $ "https://circleci.com/api/v1.1/project/github/" <> githubUser <> "/" <> githubProject <> "/"
  pure (manager, circleci_api_token, branch, base_project_url)

trigger :: FilePath -> IO ()
trigger repo = do
  ( manager, CircleciApiToken circleci_api_token, BranchName branch, Url base_url ) <- init repo
  let triggerUrl = Url $ base_url <> "build?circle-token=" <> circleci_api_token
  httpsPostJson manager triggerUrl $ BranchSelector branch

list :: FilePath -> IO ()
list repo = do
  ( manager, CircleciApiToken circleci_api_token, BranchName branch, Url base_url ) <- init repo
  let listUrl = Url $ base_url <> "tree/" <> branch <> "?circle-token=" <> circleci_api_token <> "&limit=100"
  httpsGetJson manager listUrl resultFile
  listResults

listResults :: IO ()
listResults = do
  json'' <- readFile resultFile
  let
    builds = either (panic . toS) identity $ eitherDecode (toS json'') :: [CircleciBuild]
    workflows' = groupWith (workflow_id . workflows) (builds)
  putStrLn $ T.intercalate "\n" $ (displayWorkflow <$> workflows')

  where
    displayWorkflow :: [CircleciBuild] -> Text
    displayWorkflow workflow =
      let
        p = maybe (panic "no item in workflow") identity $ head workflow
      in
        why p <> (maybe "" ((" - " <>) . login) $ user p) <> " - " <> usage_queued_at p <> " - "
          <> (T.intercalate "," $ commit <$> all_commit_details p)
          <> " - " <> status p
          <> "\n" <> "https://circleci.com/workflow-run/" <> (workflow_id . workflows $ p)
