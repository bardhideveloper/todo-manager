{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


-- | Core data types for the To‑Do Manager system.
module Types 
  ( Priority(..)
  , Status(..)
  , Task(..)
  , TaskList
  ) where

import GHC.Generics (Generic)
import Data.Aeson   (ToJSON, FromJSON)

data Priority
  = Low
  | Medium
  | High
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data Status
  = Pending
  | Completed
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

data Task = Task
  { taskId      :: Int
  , title       :: String
  , description :: String
  , priority    :: Priority
  , deadline    :: Maybe String
  , status      :: Status
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

type TaskList = [Task]