module Circleci.Types where

import           Data.Aeson (FromJSON)
import           Protolude

data CircleciBuild =
  CircleciBuild
    { status    :: Text
    , queued_at :: Text
    , workflows :: Workflow
    }
  deriving (Generic, FromJSON, Show)

data Workflow =
  Workflow
    { workflow_id :: Text
    }
  deriving (Generic, FromJSON, Show)
