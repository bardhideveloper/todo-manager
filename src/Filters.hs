-- | This module provides higher‑order filtering operations
-- over immutable TaskList structures.
module Filters
  ( filtroSipasPrioritetit,
    filtroSipasStatusit,
    kerkoDetyre,
    renditSipasPrioritetit,
    kompozoFiltra,
  )
where

import Data.List (sortOn)
import Types

-- | Return only tasks with the given priority.
filtroSipasPrioritetit :: Priority -> TaskList -> TaskList
filtroSipasPrioritetit p =
  filter (\t -> priority t == p)

-- | Return tasks that match the given status (Pending or Completed).
filtroSipasStatusit :: Status -> TaskList -> TaskList
filtroSipasStatusit s =
  filter (\t -> status t == s)

-- | Search for tasks whose title or description contains a given word.
kerkoDetyre :: String -> TaskList -> TaskList
kerkoDetyre keyword =
  filter
    ( \t ->
        let txt = title t ++ " " ++ description t
         in keyword `elem` words txt
    )

-- | Sort tasks by priority from highest to lowest.
renditSipasPrioritetit :: TaskList -> TaskList
renditSipasPrioritetit =
  reverse . sortOn priority

-- | Compose multiple filter functions into one.
-- Demonstrates higher‑order function composition.
kompozoFiltra :: [TaskList -> TaskList] -> TaskList -> TaskList
kompozoFiltra funs lista =
  foldl (\acc f -> f acc) lista funs