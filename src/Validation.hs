-- | This module contains safe parsing functions that convert String
-- input into strongly‑typed values using the Maybe type.
module Validation
  ( parseId,
    parsePriority,
    parseStatus,
  )
where

import Text.Read (readMaybe)
import Types

-- | Safely parse a numeric ID string.
parseId :: String -> Maybe Int
parseId = readMaybe

-- | Parse a priority string into a Priority ADT.
parsePriority :: String -> Maybe Priority
parsePriority str =
  case str of
    "Low" -> Just Low
    "Medium" -> Just Medium
    "High" -> Just High
    _ -> Nothing
-- | Parse a status string into a Status ADT.
parseStatus :: String -> Maybe Status
parseStatus str =
  case str of
    "Pending" -> Just Pending
    "Completed" -> Just Completed
    _ -> Nothing
