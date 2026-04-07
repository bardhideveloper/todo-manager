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

-- ADD TASK
shtoDetyre :: TaskList -> Task -> TaskList
shtoDetyre lista detyra = lista ++ [detyra]

-- REMOVE TASK
hiqDetyre :: TaskList -> Int -> TaskList
hiqDetyre lista taskIdToRemove =
  filter (\t -> taskId t /= taskIdToRemove) lista

-- UPDATE STATUS
ndryshoStatusin :: TaskList -> Int -> Status -> TaskList
ndryshoStatusin lista taskIdToUpdate statusRi =
  map
    ( \t ->
        if taskId t == taskIdToUpdate
          then t {status = statusRi}
          else t
    )
    lista

-- COMPLETE ALL TASKS
completeAll :: TaskList -> TaskList
completeAll =
  map (\t -> t {status = Completed})

-- CLEAR ALL TASKS
clearAll :: TaskList -> TaskList
clearAll _ = []