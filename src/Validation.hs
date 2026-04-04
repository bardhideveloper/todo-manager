module Validation
  ( parseId
  , parsePriority
  , parseStatus
  ) where

import Types
import Text.Read (readMaybe)

-- | Safely parse an integer ID.
parseId :: String -> Maybe Int
parseId = readMaybe

-- | Safely parse priority.
parsePriority :: String -> Maybe Priority
parsePriority str =
  case str of
    "Low"    -> Just Low
    "Medium" -> Just Medium
    "High"   -> Just High
    _        -> Nothing

-- | Safely parse status.
parseStatus :: String -> Maybe Status
parseStatus str =
  case str of
    "Pending"   -> Just Pending
    "Completed" -> Just Completed
    _           -> Nothing
