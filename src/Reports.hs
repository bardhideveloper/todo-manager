-- | This module defines functional reports derived from TaskList,
--   using compositions of filters to create declarative data pipelines.
module Reports
  ( raportoDetyratUrgjente,
    raportoDetyratPaAfat,
    raportoDetyratMeDeadline,
    Stats(..),
    computeStats
  )
where

import Data.Maybe (isJust, isNothing)
import Filters
import Types

------------------------------------------------------------
-- RAPORTE
------------------------------------------------------------

-- | Tasks that are High priority AND still Pending.
raportoDetyratUrgjente :: TaskList -> TaskList
raportoDetyratUrgjente =
  filtroSipasPrioritetit High . filtroSipasStatusit Pending

-- | Tasks that have no assigned deadline.
raportoDetyratPaAfat :: TaskList -> TaskList
raportoDetyratPaAfat =
  filter (isNothing . deadline)

-- | Tasks with a defined deadline.
raportoDetyratMeDeadline :: TaskList -> TaskList
raportoDetyratMeDeadline =
  filter (isJust . deadline)

------------------------------------------------------------
-- STATISTICS
------------------------------------------------------------

data Stats = Stats
  { totalTasks      :: Int
  , pendingTasks    :: Int
  , completedTasks  :: Int
  , highPriority    :: Int
  , mediumPriority  :: Int
  , lowPriority     :: Int
  , withoutDeadline :: Int
  , withDeadline    :: Int
  } deriving (Show)

computeStats :: TaskList -> Stats
computeStats lista =
  Stats
    { totalTasks      = length lista
    , pendingTasks    = length (filter (\t -> status t == Pending) lista)
    , completedTasks  = length (filter (\t -> status t == Completed) lista)
    , highPriority    = length (filter (\t -> priority t == High) lista)
    , mediumPriority  = length (filter (\t -> priority t == Medium) lista)
    , lowPriority     = length (filter (\t -> priority t == Low) lista)
    , withoutDeadline = length (filter (isNothing . deadline) lista)
    , withDeadline    = length (filter (isJust . deadline) lista)
    }