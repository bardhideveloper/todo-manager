-- | This module defines functional reports derived from TaskList,
--   using compositions of filters to create declarative data pipelines.
module Reports
  ( raportoDetyratUrgjente
  , raportoDetyratPaAfat
  , raportoDetyratMeDeadline
  ) where

import Filters
import Types
import Data.Maybe (isNothing, isJust)

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