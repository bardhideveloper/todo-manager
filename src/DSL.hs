module DSL
  ( Command (..),
    parseCommand,
    runCommands,
    runDSLFile,
  )
where

import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import Reports
import TaskOperations
import Types
import Validation

-- ADT: DSL Commands

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

-- Parse quoted "...".

parseQuoted :: String -> Maybe (String, String)
parseQuoted ('"' : xs) = go "" xs
  where
    go acc ('"' : rest) = Just (reverse acc, dropWhile (== ' ') rest)
    go acc (c : rest) = go (c : acc) rest
    go _ [] = Nothing
parseQuoted _ = Nothing

-- Parse ADD command

parseAdd :: String -> Maybe Command
parseAdd input =
  case words input of
    ("ADD" : idStr : rest) ->
      case parseId idStr of
        Nothing -> Nothing
        Just tid ->
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
                            Just p ->
                              let dlParsed =
                                    case parseQuoted (unwords dlRest) of
                                      Just (d, _) -> Just d
                                      Nothing -> Nothing
                               in Just (AddCmd tid titleTxt descTxt p dlParsed)
                        _ -> Nothing
    _ -> Nothing

-- Parse other commands

parseCommand :: String -> Maybe Command
parseCommand line
  | take 4 line == "ADD " = parseAdd line
  | take 5 line == "DONE " =
      case words line of
        ["DONE", idStr] -> DoneCmd <$> parseId idStr
        _ -> Nothing
  | take 7 line == "DELETE " =
      case words line of
        ["DELETE", idStr] -> DeleteCmd <$> parseId idStr
        _ -> Nothing
  | line == "REPORT urgent" = Just ReportUrgentCmd
  | "UPDATE TITLE " `isPrefixOf` line =
      let rest = drop (length "UPDATE TITLE ") line
       in case words rest of
            (idStr : _) ->
              case parseId idStr of
                Nothing -> Nothing
                Just tid ->
                  let afterId = drop (length idStr + 1) rest
                   in case parseQuoted afterId of
                        Just (newT, _) -> Just (UpdateTitleCmd tid newT)
                        Nothing -> Nothing
            _ -> Nothing
  | "UPDATE DESCRIPTION " `isPrefixOf` line =
      let rest = drop (length "UPDATE DESCRIPTION ") line
       in case words rest of
            (idStr : _) ->
              case parseId idStr of
                Nothing -> Nothing
                Just tid ->
                  let afterId = drop (length idStr + 1) rest
                   in case parseQuoted afterId of
                        Just (newD, _) -> Just (UpdateDescCmd tid newD)
                        Nothing -> Nothing
            _ -> Nothing
  | "UPDATE PRIORITY " `isPrefixOf` line =
      let rest = drop (length "UPDATE PRIORITY ") line
          ws = words rest
       in case ws of
            (idStr : priStr : _) ->
              case (parseId idStr, parsePriority priStr) of
                (Just tid, Just p) -> Just (UpdatePriorityCmd tid p)
                _ -> Nothing
            _ -> Nothing
  | line == "CLEAR completed" = Just ClearCompletedCmd
  | line == "LIST completed" = Just ListCompletedCmd
  | line == "LIST pending" = Just ListPendingCmd
  | otherwise = Nothing

-- Interpreter (pure)

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

-- Execute list of commands

runCommands :: [Command] -> TaskList -> TaskList
runCommands cmds lista =
  foldl (flip runCommand) lista cmds

-- Execute DSL file (FIXED: no file locking on Windows)

runDSLFile :: FilePath -> TaskList -> IO TaskList
runDSLFile file lista = do
  -- STRICT read (no file locking)
  bs <- BL.readFile file
  let content = T.unpack (TE.decodeUtf8 bs)

  let cmds = mapMaybe parseCommand (lines content)
  let newList = runCommands cmds lista

  putStrLn "Komandat DSL u ekzekutuan!"
  return newList