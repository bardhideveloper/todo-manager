-- | This module contains pure operations for manipulating TaskList.
-- All functions preserve immutability by returning new updated lists.
module TaskOperations
  ( shtoDetyre,
    hiqDetyre,
    ndryshoStatusin,
    completeAll,
    clearAll
  )
where

import Types

------------------------------------------------------------
-- 1. SHTO DETYRË
------------------------------------------------------------
shtoDetyre :: TaskList -> Task -> TaskList
shtoDetyre lista detyra = lista ++ [detyra]

------------------------------------------------------------
-- 2. HIQ DETYRË
------------------------------------------------------------
hiqDetyre :: TaskList -> Int -> TaskList
hiqDetyre lista taskIdToRemove =
  filter (\t -> taskId t /= taskIdToRemove) lista

------------------------------------------------------------
-- 3. NDRYSHO STATUSIN
------------------------------------------------------------
ndryshoStatusin :: TaskList -> Int -> Status -> TaskList
ndryshoStatusin lista taskIdToUpdate statusRi =
  map
    (\t ->
        if taskId t == taskIdToUpdate
          then t { status = statusRi }
          else t
    )
    lista

------------------------------------------------------------
-- 4. COMPLETE ALL TASKS (NEW FEATURE)
------------------------------------------------------------
completeAll :: TaskList -> TaskList
completeAll =
  map (\t -> t { status = Completed })

------------------------------------------------------------
-- 5. CLEAR ALL TASKS (NEW FEATURE)
------------------------------------------------------------
clearAll :: TaskList -> TaskList
clearAll _ = []