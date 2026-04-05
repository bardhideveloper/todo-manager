module Filters
  ( filtroSipasPrioritetit,
    filtroSipasStatusit,
    kerkoDetyre,
    renditSipasPrioritetit,
    kompozoFiltra,
  )
where

import Data.List (sortOn)
import Data.Ord (Down(..))
import Types

-- | Return only tasks with the given priority.
filtroSipasPrioritetit :: Priority -> TaskList -> TaskList
filtroSipasPrioritetit p =
  filter (\t -> priority t == p)

-- | Return tasks that match the given status.
filtroSipasStatusit :: Status -> TaskList -> TaskList
filtroSipasStatusit s =
  filter (\t -> status t == s)

-- | Search tasks by keyword.
kerkoDetyre :: String -> TaskList -> TaskList
kerkoDetyre keyword =
  filter
    (\t ->
        let txt = title t ++ " " ++ description t
         in keyword `elem` words txt
    )

-- ✅ | Sort tasks by priority (High → Low) using Down
renditSipasPrioritetit :: TaskList -> TaskList
renditSipasPrioritetit =
  sortOn (Down . priority)

-- | Compose multiple filters
kompozoFiltra :: [TaskList -> TaskList] -> TaskList -> TaskList
kompozoFiltra funs lista =
  foldl (\acc f -> f acc) lista funs