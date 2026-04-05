module DSL
  ( Command (..),
    parseCommand,
    runCommands,
    runDSLFile,
  )
where

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Reports
import TaskOperations
import Types
import Validation

------------------------------------------------------------
-- ADT: Komandat e DSL
------------------------------------------------------------

data Command
  = AddCmd Int String String Priority (Maybe String)
  | DoneCmd Int
  | DeleteCmd Int
  | ReportUrgentCmd
  | UpdateTitleCmd Int String
  | UpdateDescCmd Int String
  | UpdatePriorityCmd Int Priority
  | ClearCompletedCmd
  | ListCompletedCmd
  | ListPendingCmd
  deriving (Show, Eq)

------------------------------------------------------------
-- Lexon një string brenda thonjëzave "..."
------------------------------------------------------------

parseQuoted :: String -> Maybe (String, String)
parseQuoted ('"' : xs) = go "" xs
  where
    go acc ('"' : rest) = Just (reverse acc, dropWhile (== ' ') rest)
    go acc (c : rest) = go (c : acc) rest
    go _ [] = Nothing
parseQuoted _ = Nothing

------------------------------------------------------------
-- Parser për komandën ADD
-- Format:
-- ADD <id> "<title>" "<description>" <Priority> "<deadline?>"
------------------------------------------------------------

parseAdd :: String -> Maybe Command
parseAdd input =
  case words input of
    ("ADD" : idStr : rest) ->
      case parseId idStr of
        Nothing -> Nothing
        Just taskIdVal ->
          case parseQuoted (dropWhile (== ' ') (unwords rest)) of
            Nothing -> Nothing
            Just (titleTxt, rest1) ->
              case parseQuoted rest1 of
                Nothing -> Nothing
                Just (descTxt, rest2) ->
                  let ws = words rest2
                   in case ws of
                        (priStr : dlRest) ->
                          case parsePriority priStr of
                            Nothing -> Nothing
                            Just priVal ->
                              let dlParsed =
                                    case parseQuoted (unwords dlRest) of
                                      Just (d, _) -> Just d
                                      Nothing -> Nothing
                               in Just (AddCmd taskIdVal titleTxt descTxt priVal dlParsed)
                        _ -> Nothing
    _ -> Nothing

------------------------------------------------------------
-- Parser për komandat e tjera
------------------------------------------------------------

parseCommand :: String -> Maybe Command
parseCommand line
  -- ADD
  | take 4 line == "ADD " = parseAdd line
  -- DONE <id>
  | take 5 line == "DONE " =
      case words line of
        ["DONE", idStr] -> DoneCmd <$> parseId idStr
        _ -> Nothing
  -- DELETE <id>
  | take 7 line == "DELETE " =
      case words line of
        ["DELETE", idStr] -> DeleteCmd <$> parseId idStr
        _ -> Nothing
  -- REPORT urgent
  | line == "REPORT urgent" = Just ReportUrgentCmd
  -- UPDATE TITLE
  | "UPDATE TITLE " `isPrefixOf` line =
      let rest = drop (length "UPDATE TITLE ") line
       in case words rest of
            (idStr : _) ->
              case parseId idStr of
                Nothing -> Nothing
                Just taskIdVal ->
                  let afterId = drop (length idStr + 1) rest
                   in case parseQuoted afterId of
                        Just (newTitle, _) -> Just (UpdateTitleCmd taskIdVal newTitle)
                        Nothing -> Nothing
            _ -> Nothing
  -- UPDATE DESCRIPTION
  | "UPDATE DESCRIPTION " `isPrefixOf` line =
      let rest = drop (length "UPDATE DESCRIPTION ") line
       in case words rest of
            (idStr : _) ->
              case parseId idStr of
                Nothing -> Nothing
                Just taskIdVal ->
                  let afterId = drop (length idStr + 1) rest
                   in case parseQuoted afterId of
                        Just (newDesc, _) -> Just (UpdateDescCmd taskIdVal newDesc)
                        Nothing -> Nothing
            _ -> Nothing
  -- UPDATE PRIORITY
  | "UPDATE PRIORITY " `isPrefixOf` line =
      let rest = drop (length "UPDATE PRIORITY ") line
          ws = words rest
       in case ws of
            (idStr : priStr : _) ->
              case (parseId idStr, parsePriority priStr) of
                (Just taskIdVal, Just p) -> Just (UpdatePriorityCmd taskIdVal p)
                _ -> Nothing
            _ -> Nothing
  -- CLEAR completed
  | line == "CLEAR completed" = Just ClearCompletedCmd
  -- LIST completed
  | line == "LIST completed" = Just ListCompletedCmd
  -- LIST pending
  | line == "LIST pending" = Just ListPendingCmd
  -- NONE matched
  | otherwise = Nothing

------------------------------------------------------------
-- Interpreter i komandave DSL
------------------------------------------------------------

runCommand :: Command -> TaskList -> TaskList
runCommand (AddCmd tid tit desc pri dl) lista =
  shtoDetyre lista (Task tid tit desc pri dl Pending)
runCommand (DoneCmd tid) lista =
  ndryshoStatusin lista tid Completed
runCommand (DeleteCmd tid) lista =
  hiqDetyre lista tid
runCommand ReportUrgentCmd lista =
  raportoDetyratUrgjente lista
runCommand (UpdateTitleCmd tid newT) lista =
  map (\t -> if taskId t == tid then t {title = newT} else t) lista
runCommand (UpdateDescCmd tid newD) lista =
  map (\t -> if taskId t == tid then t {description = newD} else t) lista
runCommand (UpdatePriorityCmd tid newP) lista =
  map (\t -> if taskId t == tid then t {priority = newP} else t) lista
runCommand ClearCompletedCmd lista =
  filter (\t -> status t /= Completed) lista
runCommand ListCompletedCmd lista =
  filter (\t -> status t == Completed) lista
runCommand ListPendingCmd lista =
  filter (\t -> status t == Pending) lista

------------------------------------------------------------
-- Ekzekutimi i një liste komandash DSL
------------------------------------------------------------

runCommands :: [Command] -> TaskList -> TaskList
runCommands cmds lista =
  foldl (flip runCommand) lista cmds

------------------------------------------------------------
-- Ekzekutimi i një file DSL
------------------------------------------------------------

runDSLFile :: FilePath -> TaskList -> IO TaskList
runDSLFile file lista = do
  content <- readFile file
  let cmds = mapMaybe parseCommand (lines content)
  let newList = runCommands cmds lista
  putStrLn "Komandat DSL u ekzekutuan!"
  return newList