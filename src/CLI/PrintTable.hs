module CLI.PrintTable
  ( printList,
    printTask,
  )
where

import CLI.Colors (colorCyan)
import Types

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

pad :: Int -> String -> String
pad n s =
  let s' = take n s
      len = length s'
   in s' ++ replicate (n - len) ' '

-- Split a string into chunks of fixed width
wrap :: Int -> String -> [String]
wrap _ [] = []
wrap n s =
  let (l, r) = splitAt n s
   in l : wrap n r

------------------------------------------------------------
-- PRINT SINGLE TASK (MULTILINE)
------------------------------------------------------------

printTask :: Task -> IO ()
printTask t = do
  let titleLines = wrap 30 (title t)
      descLines = wrap 50 (description t)
      maxLines = max (length titleLines) (length descLines)

      getChunk xs i = if i < length xs then xs !! i else ""

  mapM_
    ( \i ->
        putStrLn $
          "| "
            ++ pad 4 (if i == 0 then show (taskId t) else "")
            ++ "| "
            ++ pad 30 (getChunk titleLines i)
            ++ "| "
            ++ pad 50 (getChunk descLines i)
            ++ "| "
            ++ pad 8 (if i == 0 then show (priority t) else "")
            ++ "| "
            ++ pad 12 (if i == 0 then show (deadline t) else "")
            ++ "| "
            ++ pad 10 (if i == 0 then show (status t) else "")
            ++ "|"
    )
    [0 .. maxLines - 1]

------------------------------------------------------------
-- PRINT LIST WITH HEADER
------------------------------------------------------------

printList :: TaskList -> IO ()
printList lista = do
  putStrLn colorCyan
  putStrLn "+----+--------------------------------+--------------------------------------------------+----------+--------------+------------+"
  putStrLn "| ID | Title                          | Description                                      | Priority | Deadline     | Status     |"
  putStrLn "+----+--------------------------------+--------------------------------------------------+----------+--------------+------------+"
  mapM_ printTask lista
  putStrLn "+----+--------------------------------+--------------------------------------------------+----------+--------------+------------+"
  putStrLn "\x1b[0m"