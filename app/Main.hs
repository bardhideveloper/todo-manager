module Main where

import Types
import Persistence
import CLI.Menu (startMenu)
import CLI.Colors (colorCyan, colorReset)

main :: IO ()
main = do
    putStrLn $ colorCyan ++ "Sistemi u startua!\n" ++ colorReset
    lista <- loadTasks "tasks.db"
    startMenu lista