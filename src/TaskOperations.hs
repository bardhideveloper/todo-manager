module TaskOperations
  ( shtoDetyre
  , hiqDetyre
  , ndryshoStatusin
  ) where

import Types

-- | Adds a new task to the task list.
shtoDetyre :: TaskList -> Task -> TaskList
shtoDetyre lista detyra = lista ++ [detyra]

-- | Removes a task by its ID.
hiqDetyre :: TaskList -> Int -> TaskList
hiqDetyre lista taskIdToRemove =
  filter (\t -> taskId t /= taskIdToRemove) lista

-- | Updates the status of a task with the given ID.
ndryshoStatusin :: TaskList -> Int -> Status -> TaskList
ndryshoStatusin lista taskIdToUpdate statusRi =
  map (\t -> if taskId t == taskIdToUpdate
             then t { status = statusRi }
             else t)
      lista