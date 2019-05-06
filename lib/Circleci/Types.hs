module Circleci.Types where

import           Data.Aeson
import           Protolude

newtype CircleciApiToken = CircleciApiToken [Char]

data CircleciBuild = CircleciBuild{
  why                :: Text,
  user               :: Maybe CircleciUser,
  usage_queued_at    :: Text,
  status             :: Text,
  workflows          :: WorkflowReference,
  all_commit_details :: [CommitDetail]
} deriving (Generic,FromJSON)

data CircleciUser = CircleciUser{
  login :: Text
} deriving (Generic,FromJSON)

data CommitDetail = CommitDetail{
  commit :: Text
} deriving (Generic,FromJSON)

data WorkflowReference = WorkflowReference{
  workflow_id :: Text
} deriving (Generic,FromJSON)

data BranchSelector = BranchSelector{
  branch :: [Char]
} deriving (Generic, ToJSON)
