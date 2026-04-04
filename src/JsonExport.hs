-- | JSON export/import using the Aeson library.
module JsonExport
  ( exportToJson
  , importFromJson
  ) where

import Types
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL

-- | Export tasks to a JSON file.
exportToJson :: FilePath -> TaskList -> IO ()
exportToJson file lista = do
    BL.writeFile file (encode lista)
    putStrLn "Detyrat u eksportuan ne JSON!"

-- | Import tasks from a JSON file.
-- If file missing or corrupted, return [].
importFromJson :: FilePath -> IO TaskList
importFromJson file = do
    content <- BL.readFile file
    case eitherDecode content of
        Left _    -> do
            putStrLn "JSON i pavlefshem ose file mungon!"
            return []
        Right tasks -> do
            putStrLn "JSON u importua me sukses!"
            return tasks