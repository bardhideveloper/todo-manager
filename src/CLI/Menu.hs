module CLI.Menu
  ( startMenu,
  )
where

import CLI.Colors
import CLI.Handlers (handleChoice)
import Persistence
import System.IO (hFlush, stdout)
import Types

startMenu :: TaskList -> IO ()
startMenu lista = do
  putStrLn $ colorCyan ++ "===========================================" ++ colorReset
  putStrLn $ colorGreen ++ "           TO-DO MANAGER (CLI)" ++ colorReset
  putStrLn $ colorCyan ++ "===========================================\n" ++ colorReset

  putStrLn $ colorYellow ++ "  [1] -> Shto detyre" ++ colorReset
  putStrLn $ colorYellow ++ "  [2] -> Hiq detyre" ++ colorReset
  putStrLn $ colorYellow ++ "  [3] -> Ndrysho statusin" ++ colorReset
  putStrLn $ colorYellow ++ "  [4] -> Shfaq te gjitha detyrat" ++ colorReset

  putStrLn $ colorBlue ++ "\n------------ Filtra & Kerkime ------------" ++ colorReset
  putStrLn "  [5] -> Filtra & Kerkime"
  putStrLn "  [6] -> Raporte"

  putStrLn $ colorBlue ++ "\n------------ Import / Export ------------" ++ colorReset
  putStrLn "  [7] -> Eksporto JSON"
  putStrLn "  [8] -> Importo JSON"
  putStrLn "  [9] -> Eksporto HTML"

  putStrLn $ colorBlue ++ "\n------------------ DSL ------------------" ++ colorReset
  putStrLn "  [10] -> Ekzekuto DSL"

  putStrLn $ colorBlue ++ "\n----------------- STATISTIKA -----------------" ++ colorReset
  putStrLn "  [11] -> Shfaq Statistikat"

  putStrLn $ colorBlue ++ "\n----------------- OPERACIONE SHTESE -----------------" ++ colorReset
  putStrLn "  [12] -> Perfundo te gjitha detyrat"
  putStrLn "  [13] -> Fshij te gjitha detyrat"

  putStrLn $ colorRed ++ "\n  [0] -> Dil nga programi\n" ++ colorReset

  putStr "Zgjedhja: "
  hFlush stdout
  choice <- getLine

  if choice == "0"
    then do
      putStrLn $ colorGreen ++ "Ruajtja ne tasks.db..." ++ colorReset
      saveTasks "tasks.db" lista
      putStrLn "Dalje..."
    else do
      lista' <- handleChoice choice lista
      startMenu lista'