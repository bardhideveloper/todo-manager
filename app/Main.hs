module Main where

import CLI.Colors (colorCyan, colorReset)
import CLI.Menu (startMenu)
import Persistence

main :: IO ()
main = do
  putStrLn $ colorCyan ++ "Sistemi u startua!\n" ++ colorReset
  lista <- loadTasks "tasks.db"
  startMenu lista