module DSL
  ( Command(..)
  , parseCommand
  , runCommands
  , runDSLFile
  ) where

import Types
import TaskOperations
import Reports
import Validation

------------------------------------------------------------
-- ADT: Komandat e DSL
------------------------------------------------------------

data Command
  = AddCmd Int String String Priority (Maybe String)
  | DoneCmd Int
  | DeleteCmd Int
  | ReportUrgentCmd
  deriving (Show, Eq)

------------------------------------------------------------
-- PARSER: Leximi i një rreshti të DSL
------------------------------------------------------------

parseCommand :: String -> Maybe Command
parseCommand line =
  case words line of
    -- ADD id tit desc priority [deadline]
    ("ADD" : idStr : tit : desc : pri : dl) ->
      case (parseId idStr, parsePriority pri) of
        (Just idVal, Just prVal) ->
          let deadline =
                case dl of
                  []    -> Nothing
                  (d:_) -> Just d
           in Just (AddCmd idVal tit desc prVal deadline)
        _ -> Nothing

    -- DONE id
    ["DONE", idStr] ->
      case parseId idStr of
        Just idVal -> Just (DoneCmd idVal)
        Nothing -> Nothing

    -- DELETE id
    ["DELETE", idStr] ->
      case parseId idStr of
        Just idVal -> Just (DeleteCmd idVal)
        Nothing -> Nothing

    -- REPORT urgent
    ["REPORT", "urgent"] ->
      Just ReportUrgentCmd

    _ -> Nothing

------------------------------------------------------------
-- INTERPRETER: Ekzekutimi i një komande
------------------------------------------------------------

runCommand :: Command -> TaskList -> TaskList
runCommand (AddCmd id tit desc pri dl) lista =
  shtoDetyre lista (Task id tit desc pri dl Pending)

runCommand (DoneCmd id) lista =
  ndryshoStatusin lista id Completed

runCommand (DeleteCmd id) lista =
  hiqDetyre lista id

runCommand ReportUrgentCmd lista =
  raportoDetyratUrgjente lista

------------------------------------------------------------
-- Ekzekutimi i disa komandave rresht për rresht
------------------------------------------------------------

runCommands :: [Command] -> TaskList -> TaskList
runCommands cmds lista =
  foldl (\acc c -> runCommand c acc) lista cmds

------------------------------------------------------------
-- Ekzekutimi i një file-i DSL
------------------------------------------------------------

runDSLFile :: FilePath -> TaskList -> IO TaskList
runDSLFile file lista = do
  content <- readFile file
  let ls = lines content
  let cmds = map parseCommand ls
  let validCmds = [c | Just c <- cmds]
  let newList = runCommands validCmds lista
  putStrLn "Komandat DSL u ekzekutuan!"
  return newList