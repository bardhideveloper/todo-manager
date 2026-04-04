module Persistence
  ( saveTasks,
    loadTasks,
  )
where

import System.Directory (doesFileExist)
import Types

-- | Save the current TaskList to a file.
saveTasks :: FilePath -> TaskList -> IO ()
saveTasks file lista = do
  writeFile file (show lista)
  putStrLn "Detyrat u ruajten me sukses!"

-- | Load TaskList back from a file.
-- If the file does not exist or is empty, return an empty list.
loadTasks :: FilePath -> IO TaskList
loadTasks file = do
  exists <- doesFileExist file
  if not exists
    then return []
    else do
      content <- readFile file
      if null content
        then return []
        else return (read content :: TaskList)