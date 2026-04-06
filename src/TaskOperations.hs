-- | This module contains pure operations for manipulating TaskList.
-- All functions preserve immutability by returning new updated lists.
module TaskOperations
  ( shtoDetyre,
    hiqDetyre,
    ndryshoStatusin,
    completeAll,
    clearAll,
  )
where

import Types

------------------------------------------------------------
-- 1. ADD TASK
------------------------------------------------------------
shtoDetyre :: TaskList -> Task -> TaskList
shtoDetyre lista detyra = lista ++ [detyra]

------------------------------------------------------------
-- 2. REMOVE TASK
------------------------------------------------------------
hiqDetyre :: TaskList -> Int -> TaskList
hiqDetyre lista taskIdToRemove =
  filter (\t -> taskId t /= taskIdToRemove) lista

------------------------------------------------------------
-- 3. UPDATE STATUS
------------------------------------------------------------
ndryshoStatusin :: TaskList -> Int -> Status -> TaskList
ndryshoStatusin lista taskIdToUpdate statusRi =
  map
    ( \t ->
        if taskId t == taskIdToUpdate
          then t {status = statusRi}
          else t
    )
    lista

------------------------------------------------------------
-- 4. COMPLETE ALL TASKS
------------------------------------------------------------
completeAll :: TaskList -> TaskList
completeAll =
  map (\t -> t {status = Completed})

------------------------------------------------------------
-- 5. CLEAR ALL TASKS
------------------------------------------------------------
clearAll :: TaskList -> TaskList
clearAll _ = []