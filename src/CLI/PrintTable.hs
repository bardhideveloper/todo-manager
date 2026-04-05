module CLI.PrintTable
  ( printList
  , printTask
  ) where

import Types
import CLI.Colors (colorCyan, colorReset)

pad :: Int -> String -> String
pad n s =
    let s' = take n s
        len = length s'
    in s' ++ replicate (n - len) ' '

printTask :: Task -> IO ()
printTask t = do
    putStrLn $
         "| " ++ pad 4 (show $ taskId t)
      ++ "| " ++ pad 18 (title t)
      ++ "| " ++ pad 25 (description t)
      ++ "| " ++ pad 8 (show $ priority t)
      ++ "| " ++ pad 12 (show $ deadline t)
      ++ "| " ++ pad 10 (show $ status t)
      ++ "|"

printList :: TaskList -> IO ()
printList lista = do
    putStrLn colorCyan
    putStrLn "+----+------------------+---------------------------+----------+--------------+------------+"
    putStrLn "| ID | Title            | Description               | Priority | Deadline     | Status     |"
    putStrLn "+----+------------------+---------------------------+----------+--------------+------------+"
    mapM_ printTask lista
    putStrLn "+----+------------------+---------------------------+----------+--------------+------------+"
    putStrLn colorReset