module Reports
  ( raportoDetyratUrgjente
  , raportoDetyratPaAfat
  , raportoDetyratMeDeadline
  ) where

import Types
import Filters

-- | Reports all tasks that are high priority AND still pending.
-- Demonstrates composition of pure filters.
raportoDetyratUrgjente :: TaskList -> TaskList
raportoDetyratUrgjente =
    (filtroSipasPrioritetit High . filtroSipasStatusit Pending)

-- | Reports tasks that have no deadline (deadline = Nothing).
raportoDetyratPaAfat :: TaskList -> TaskList
raportoDetyratPaAfat =
    filter (\t -> deadline t == Nothing)

-- | Reports tasks that DO have a deadline.
raportoDetyratMeDeadline :: TaskList -> TaskList
raportoDetyratMeDeadline =
    filter (\t -> deadline t /= Nothing)