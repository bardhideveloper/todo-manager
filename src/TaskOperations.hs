-- | This module contains pure operations for manipulating TaskList.
-- All functions preserve immutability by returning new updated lists.
module TaskOperations
  ( shtoDetyre,
    hiqDetyre,
    ndryshoStatusin,
  )
where

import Types

-- | Add a new task to the end of a task list.
-- Pure function: does not modify the original list.
shtoDetyre :: TaskList -> Task -> TaskList
shtoDetyre lista detyra = lista ++ [detyra]

-- | Remove a task from the list by its ID.
-- If no task with the given ID exists, the list is returned unchanged.
hiqDetyre :: TaskList -> Int -> TaskList
hiqDetyre lista taskIdToRemove =
  filter (\t -> taskId t /= taskIdToRemove) lista

-- | Update the status of a specific task by ID.
-- If no matching task exists, the list is returned unchanged.
ndryshoStatusin :: TaskList -> Int -> Status -> TaskList
ndryshoStatusin lista taskIdToUpdate statusRi =
  map
    ( \t ->
        if taskId t == taskIdToUpdate
          then t {status = statusRi}
          else t
    )
    lista