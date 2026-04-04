module Persistence
  ( saveTasks
  , loadTasks
  ) where

import Types
import System.Directory (doesFileExist)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- | Save the TaskList strictly to avoid file locks on Windows.
saveTasks :: FilePath -> TaskList -> IO ()
saveTasks file lista = do
    let content = show lista
    B.writeFile file (BC.pack content)
    putStrLn "Detyrat u ruajten me sukses!"

-- | Load TaskList strictly using ByteString to avoid lazy file locking.
loadTasks :: FilePath -> IO TaskList
loadTasks file = do
    exists <- doesFileExist file
    if not exists
       then return []
       else do
           bytes <- B.readFile file
           let content = BC.unpack bytes
           if null content
               then return []
               else return (read content :: TaskList)