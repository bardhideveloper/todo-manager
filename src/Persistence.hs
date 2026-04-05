module Persistence
  ( saveTasks,
    loadTasks,
  )
where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Types

------------------------------------------------------------
-- Ruaj TaskList në tasks.db (si JSON)
------------------------------------------------------------
saveTasks :: FilePath -> TaskList -> IO ()
saveTasks file lista = BL.writeFile file (encode lista)

------------------------------------------------------------
-- Lexo TaskList nga tasks.db (JSON)
------------------------------------------------------------
loadTasks :: FilePath -> IO TaskList
loadTasks file = do
  content <- BL.readFile file
  case eitherDecode content of
    Left _ -> return [] -- file bosh, invalid, ose mungon
    Right ts -> return ts