module DSL
  ( Command (..),
    parseCommand,
    runCommands,
    runDSLFile,
  )
where

import Reports
import TaskOperations
import Types
import Validation
import Data.List (isPrefixOf)

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
-- Helper: Lexon një string të rrethuar me thonjëza
------------------------------------------------------------

parseQuoted :: String -> Maybe (String, String)
parseQuoted ('"' : xs) = go "" xs
  where
    go acc ('"' : rest) = Just (reverse acc, dropWhile (== ' ') rest)
    go acc (c : rest) = go (c : acc) rest
    go _ [] = Nothing
parseQuoted _ = Nothing

------------------------------------------------------------
-- Parser për ADD me thonjëza
-- Formati:
-- ADD <id> "<title>" "<description>" <Priority> "<deadline?>"
------------------------------------------------------------

parseAdd :: String -> Maybe Command
parseAdd input =
  case words input of
    ("ADD" : idStr : rest) ->
      case parseId idStr of
        Nothing -> Nothing
        Just idVal ->
          -- Lexo title
          case parseQuoted (dropWhile (== ' ') (unwords rest)) of
            Nothing -> Nothing
            Just (title, rest1) ->
              -- Lexo description
              case parseQuoted rest1 of
                Nothing -> Nothing
                Just (desc, rest2) ->
                  let ws = words rest2
                   in case ws of
                        (priStr : dlRest) ->
                          case parsePriority priStr of
                            Nothing -> Nothing
                            Just prVal ->
                              let deadline =
                                    case parseQuoted (unwords dlRest) of
                                      Just (d, _) -> Just d
                                      Nothing -> Nothing
                               in Just (AddCmd idVal title desc prVal deadline)
                        _ -> Nothing
    _ -> Nothing

------------------------------------------------------------
-- Parser për komandat e tjera
------------------------------------------------------------

parseCommand :: String -> Maybe Command
parseCommand line
  -- ADD
  | take 4 line == "ADD " = parseAdd line
  -- DONE ID
  | "DONE " `elem` [take 5 line] =
      case words line of
        ["DONE", idStr] -> DoneCmd <$> parseId idStr
        _ -> Nothing
  -- DELETE ID
  | "DELETE " `elem` [take 7 line] =
      case words line of
        ["DELETE", idStr] -> DeleteCmd <$> parseId idStr
        _ -> Nothing
  -- REPORT urgent
  | line == "REPORT urgent" = Just ReportUrgentCmd
------------------------------------------------------------
-- UPDATE TITLE <id> "New Title"
------------------------------------------------------------
parseCommand line
  | "UPDATE TITLE " `isPrefixOf` line =
      case drop (length "UPDATE TITLE ") line of
        rest ->
          case words rest of
            (idStr : _) ->
              case parseId idStr of
                Nothing -> Nothing
                Just idVal ->
                  let afterId = drop (length idStr + 1) rest
                   in case parseQuoted afterId of
                        Just (newTitle, _) -> Just (UpdateTitleCmd idVal newTitle)
                        Nothing -> Nothing
            _ -> Nothing
------------------------------------------------------------
-- UPDATE DESCRIPTION <id> "New Description"
------------------------------------------------------------
parseCommand line
  | "UPDATE DESCRIPTION " `isPrefixOf` line =
      case drop (length "UPDATE DESCRIPTION ") line of
        rest ->
          case words rest of
            (idStr : _) ->
              case parseId idStr of
                Nothing -> Nothing
                Just idVal ->
                  let afterId = drop (length idStr + 1) rest
                   in case parseQuoted afterId of
                        Just (newDesc, _) -> Just (UpdateDescCmd idVal newDesc)
                        Nothing -> Nothing
            _ -> Nothing
------------------------------------------------------------
-- UPDATE PRIORITY <id> <Low|Medium|High>
------------------------------------------------------------
parseCommand line
  | "UPDATE PRIORITY " `isPrefixOf` line =
      let rest = drop (length "UPDATE PRIORITY ") line
          parts = words rest
       in case parts of
            (idStr : priStr : _) ->
              case (parseId idStr, parsePriority priStr) of
                (Just idVal, Just p) -> Just (UpdatePriorityCmd idVal p)
                _ -> Nothing
            _ -> Nothing
  -- CLEAR completed
  | line == "CLEAR completed" = Just ClearCompletedCmd
  -- LIST completed
  | line == "LIST completed" = Just ListCompletedCmd
  -- LIST pending
  | line == "LIST pending" = Just ListPendingCmd
  -- Nothing matched
  | otherwise = Nothing

------------------------------------------------------------
-- Interpreter: Ekzekutimi i komandave DSL
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
runCommand (UpdateTitleCmd id newT) lista =
  map (\t -> if taskId t == id then t {title = newT} else t) lista
runCommand (UpdateDescCmd id newD) lista =
  map (\t -> if taskId t == id then t {description = newD} else t) lista
runCommand (UpdatePriorityCmd id newP) lista =
  map (\t -> if taskId t == id then t {priority = newP} else t) lista
runCommand ClearCompletedCmd lista =
  filter (\t -> status t /= Completed) lista
runCommand ListCompletedCmd lista =
  filter (\t -> status t == Completed) lista
runCommand ListPendingCmd lista =
  filter (\t -> status t == Pending) lista

------------------------------------------------------------
-- Ekzekutimi i një liste komandash
------------------------------------------------------------

runCommands :: [Command] -> TaskList -> TaskList
runCommands cmds lista = foldl (flip runCommand) lista cmds

------------------------------------------------------------
-- Ekzekuto një file DSL
------------------------------------------------------------

runDSLFile :: FilePath -> TaskList -> IO TaskList
runDSLFile file lista = do
  content <- readFile file
  let ls = lines content
  let cmds = [c | Just c <- map parseCommand ls]
  let newList = runCommands cmds lista
  putStrLn "Komandat DSL u ekzekutuan (version me update)!"
  return newList