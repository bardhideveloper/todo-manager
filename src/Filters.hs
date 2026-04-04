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

-- | Filters tasks by a given priority.
filtroSipasPrioritetit :: Priority -> TaskList -> TaskList
filtroSipasPrioritetit p =
  filter (\t -> priority t == p)

-- | Filters tasks based on their status (Pending / Completed).
filtroSipasStatusit :: Status -> TaskList -> TaskList
filtroSipasStatusit s =
  filter (\t -> status t == s)

-- | Searches for tasks containing a given keyword in title or description.
kerkoDetyre :: String -> TaskList -> TaskList
kerkoDetyre keyword =
  filter
    ( \t ->
        let txt = title t ++ " " ++ description t
         in keyword `elem` words txt
    )

-- | Sorts tasks from highest to lowest priority.
renditSipasPrioritetit :: TaskList -> TaskList
renditSipasPrioritetit =
  reverse . sortOn priority

-- | Composes multiple filter functions into a single pipeline.
-- This demonstrates higher-order function usage.
kompozoFiltra :: [TaskList -> TaskList] -> TaskList -> TaskList
kompozoFiltra funs lista =
  foldl (\acc f -> f acc) lista funs