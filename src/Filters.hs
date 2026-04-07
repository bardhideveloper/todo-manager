module Filters
  ( filtroSipasPrioritetit,
    filtroSipasStatusit,
    kerkoDetyre,
    renditSipasPrioritetit,
    kompozoFiltra
  )
where

import Data.List (sortOn, isInfixOf)
import Data.Ord (Down (..))
import Data.Char (toLower)
import Types

-- FILTERS

-- Return only tasks with the given priority.
filtroSipasPrioritetit :: Priority -> TaskList -> TaskList
filtroSipasPrioritetit p =
  filter (\t -> priority t == p)

-- Return tasks that match the given status.
filtroSipasStatusit :: Status -> TaskList -> TaskList
filtroSipasStatusit s =
  filter (\t -> status t == s)

-- SEARCH (FIXED VERSION)

--Search tasks by keyword (case-insensitive, substring search)
--   Searches in title and description.
kerkoDetyre :: String -> TaskList -> TaskList
kerkoDetyre keyword =
  filter
    (\t ->
      let kw  = map toLower keyword
          txt = map toLower (title t ++ " " ++ description t)
      in kw `isInfixOf` txt
    )

-- SORTING

--Sort tasks by priority (High → Low)
renditSipasPrioritetit :: TaskList -> TaskList
renditSipasPrioritetit =
  sortOn (Down . priority)

-- COMPOSITION

--Compose multiple filters into a single pipeline
kompozoFiltra :: [TaskList -> TaskList] -> TaskList -> TaskList
kompozoFiltra funs lista =
  foldl (\acc f -> f acc) lista funs