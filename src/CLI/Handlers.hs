module CLI.Handlers
  ( handleChoice
  ) where

import CLI.Colors
import CLI.PrintTable
import DSL
import Filters
import HtmlExport
import JsonExport
import Reports
import System.IO (hFlush, stdout)
import TaskOperations
import Types
import Validation

------------------------------------------------------------
-- INPUT HELPER
------------------------------------------------------------

prompt :: String -> IO String
prompt text = do
  putStr text
  putStr " "
  hFlush stdout
  getLine

------------------------------------------------------------
-- CONFIRMATION HELPER
------------------------------------------------------------

confirm :: String -> IO Bool
confirm msg = do
  putStr (msg ++ " (yes/no): ")
  hFlush stdout
  ans <- getLine
  return (ans == "yes")

------------------------------------------------------------
-- HANDLE CHOICE
------------------------------------------------------------

handleChoice :: String -> TaskList -> IO TaskList

-- ADD TASK
handleChoice "1" lista = cliShtoDetyre lista

-- DELETE TASK (WITH CONFIRMATION)
handleChoice "2" lista = cliHiq lista

-- CHANGE STATUS
handleChoice "3" lista = cliNdrysho lista

-- SHOW ALL TASKS
handleChoice "4" lista = do
  printList lista
  return lista

-- FILTERS
handleChoice "5" lista = cliFiltra lista >> return lista

-- REPORTS
handleChoice "6" lista = cliRaporte lista >> return lista

------------------------------------------------------------
-- IMPORT / EXPORT
------------------------------------------------------------

-- JSON EXPORT
handleChoice "7" lista = do
  exportToJson "tasks_export.json" lista
  putStrLn $ colorGreen ++ "Detyrat u eksportuan ne JSON!" ++ colorReset
  return lista

-- JSON IMPORT (lista e vjeter nuk perdoret)
handleChoice "8" _ = do
  lista' <- importFromJson "tasks.json"
  putStrLn $ colorGreen ++ "JSON u importua me sukses!" ++ colorReset
  return lista'

-- HTML EXPORT
handleChoice "9" lista = do
  exportHtml "tasks.html" lista
  putStrLn $ colorGreen ++ "Detyrat u eksportuan ne HTML!" ++ colorReset
  return lista

------------------------------------------------------------
-- DSL
------------------------------------------------------------

handleChoice "10" lista = do
  file <- prompt "Shkruaj emrin e file DSL:"
  lista' <- runDSLFile file lista
  putStrLn $ colorGreen ++ "Gjendja e detyrave pas DSL:" ++ colorReset
  printList lista'
  return lista'

------------------------------------------------------------
-- STATISTICS
------------------------------------------------------------

handleChoice "11" lista = do
  let s = computeStats lista
  putStrLn $ colorBlue ++ "\n--- STATISTIKA ---\n" ++ colorReset
  putStrLn $ "Totali i detyrave:      " ++ show (totalTasks s)
  putStrLn $ "Ne pritje:               " ++ show (pendingTasks s)
  putStrLn $ "Te perfunduara:          " ++ show (completedTasks s)
  putStrLn $ "Prioritet High:          " ++ show (highPriority s)
  putStrLn $ "Prioritet Medium:        " ++ show (mediumPriority s)
  putStrLn $ "Prioritet Low:           " ++ show (lowPriority s)
  putStrLn $ "Pa afat:                 " ++ show (withoutDeadline s)
  putStrLn $ "Me afat:                 " ++ show (withDeadline s)
  return lista

------------------------------------------------------------
-- EXTRA OPERATIONS
------------------------------------------------------------

-- MARK ALL AS COMPLETED (WITH CONFIRMATION)
handleChoice "12" lista = do
  ok <- confirm "A je i sigurt qe do i perfundosh te gjitha detyrat?"
  if ok
    then do
      putStrLn $ colorGreen ++ "Te gjitha detyrat u shenuan si Completed!" ++ colorReset
      return (completeAll lista)
    else do
      putStrLn $ colorBlue ++ "Operacioni u anulua." ++ colorReset
      return lista

-- CLEAR ALL TASKS (WITH CONFIRMATION)
handleChoice "13" lista = do
  ok <- confirm "A je i sigurt qe do fshish te gjitha detyrat?"
  if ok
    then do
      putStrLn $ colorYellow ++ "Te gjitha detyrat u fshine!" ++ colorReset
      return (clearAll lista)
    else do
      putStrLn $ colorBlue ++ "Operacioni u anulua." ++ colorReset
      return lista

------------------------------------------------------------
-- DEFAULT
------------------------------------------------------------

handleChoice _ lista = do
  putStrLn $ colorRed ++ "Zgjedhje e pasakte! Ju lutem provoni perseri." ++ colorReset
  return lista

------------------------------------------------------------
-- CLI COMMAND IMPLEMENTATIONS
------------------------------------------------------------

cliShtoDetyre :: TaskList -> IO TaskList
cliShtoDetyre lista = do
  idStr <- prompt "ID:"
  tit   <- prompt "Titulli:"
  desc  <- prompt "Pershkrimi:"
  prStr <- prompt "Prioriteti (Low/Medium/High):"
  dl    <- prompt "Afati (ose Enter per asnje):"

  let maybeId = parseId idStr
  let maybePr = parsePriority prStr

  case (maybeId, maybePr) of
    (Just idVal, Just prVal) -> do
      let deadlineVal = if null dl then Nothing else Just dl
      let t = Task idVal tit desc prVal deadlineVal Pending
      putStrLn $ colorGreen ++ "Detyra u shtua!" ++ colorReset
      return (shtoDetyre lista t)
    _ -> do
      putStrLn $ colorRed ++ "Gabim ne input!" ++ colorReset
      return lista

cliHiq :: TaskList -> IO TaskList
cliHiq lista = do
  idStr <- prompt "ID e detyres per heqje:"
  case parseId idStr of
    Nothing -> do
      putStrLn $ colorRed ++ "ID e pasakte!" ++ colorReset
      return lista
    Just idVal -> do
      ok <- confirm "A je i sigurt qe do e heqesh kete detyre?"
      if ok
        then do
          putStrLn $ colorYellow ++ "Detyra u hoq!" ++ colorReset
          return (hiqDetyre lista idVal)
        else do
          putStrLn $ colorBlue ++ "Operacioni u anulua." ++ colorReset
          return lista

cliNdrysho :: TaskList -> IO TaskList
cliNdrysho lista = do
  idStr <- prompt "ID e detyres:"
  sStr  <- prompt "Status (Pending/Completed):"

  case (parseId idStr, parseStatus sStr) of
    (Just idVal, Just statusVal) -> do
      putStrLn $ colorGreen ++ "Statusi u ndryshua!" ++ colorReset
      return (ndryshoStatusin lista idVal statusVal)
    _ -> do
      putStrLn $ colorRed ++ "Gabim ne input!" ++ colorReset
      return lista

------------------------------------------------------------
-- FILTERS
------------------------------------------------------------

cliFiltra :: TaskList -> IO ()
cliFiltra lista = do
  putStrLn $ colorBlue ++ "\n--- Filtra & Kerkime ---" ++ colorReset
  putStrLn "1. Filtra sipas prioritetit"
  putStrLn "2. Filtra sipas statusit"
  putStrLn "3. Kerkim me fjale"
  putStrLn "4. Rendit sipas prioritetit"
  putStrLn "0. Kthehu"

  choice <- prompt "Zgjedhja:"

  case choice of
    "1" -> do
      p <- prompt "Prioriteti:"
      case parsePriority p of
        Just pv -> printList (filtroSipasPrioritetit pv lista)
        Nothing -> putStrLn $ colorRed ++ "Prioritet i pasakte!" ++ colorReset
    "2" -> do
      s <- prompt "Statusi:"
      case parseStatus s of
        Just sv -> printList (filtroSipasStatusit sv lista)
        Nothing -> putStrLn $ colorRed ++ "Status i pasakte!" ++ colorReset
    "3" -> do
      w <- prompt "Fjala per kerkimin:"
      printList (kerkoDetyre w lista)
    "4" -> printList (renditSipasPrioritetit lista)
    "0" -> return ()
    _   -> putStrLn $ colorRed ++ "Zgjedhje e pasakte!" ++ colorReset

------------------------------------------------------------
-- REPORTS
------------------------------------------------------------

cliRaporte :: TaskList -> IO ()
cliRaporte lista = do
  putStrLn $ colorBlue ++ "\n--- Raporte ---" ++ colorReset
  putStrLn "1. Detyrat urgjente (High & Pending)"
  putStrLn "2. Detyrat pa afat"
  putStrLn "3. Detyrat me afat"
  putStrLn "0. Kthehu"

  choice <- prompt "Zgjedhja:"

  case choice of
    "1" -> printList (raportoDetyratUrgjente lista)
    "2" -> printList (raportoDetyratPaAfat lista)
    "3" -> printList (raportoDetyratMeDeadline lista)
    "0" -> return ()
    _   -> putStrLn $ colorRed ++ "Zgjedhje e pasakte!" ++ colorReset