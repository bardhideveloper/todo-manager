module Persistence
  ( saveTasks,
    loadTasks,
  )
where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import Types

-- Save tasks strictly (NO locking)
saveTasks :: FilePath -> TaskList -> IO ()
saveTasks file lista =
  BL.writeFile file (encode lista)

-- Load tasks strictly (NO lazy readFile)
loadTasks :: FilePath -> IO TaskList
loadTasks file = do
  fileExists <- doesFileExist file
  if not fileExists
    then return []
    else do
      content <- BL.readFile file
      case eitherDecode content of
        Left _ -> return []
        Right t -> return t