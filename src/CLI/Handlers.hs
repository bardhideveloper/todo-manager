{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module CLI.Handlers
  ( handleChoice,
  )
where

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
-- HANDLE CHOICE
------------------------------------------------------------

handleChoice :: String -> TaskList -> IO TaskList
handleChoice "1" lista = cliShtoDetyre lista
handleChoice "2" lista = cliHiq lista
handleChoice "3" lista = cliNdrysho lista
handleChoice "4" lista = printList lista >> return lista
handleChoice "5" lista = cliFiltra lista >> return lista
handleChoice "6" lista = cliRaporte lista >> return lista
-- JSON EXPORT
handleChoice "7" lista = do
  exportToJson "tasks.json" lista
  putStrLn $ colorGreen ++ "Detyrat u eksportuan ne JSON!" ++ colorReset
  return lista

-- JSON IMPORT
handleChoice "4" lista = do
  printList lista
  return lista

-- HTML EXPORT
handleChoice "9" lista = do
  exportHtml "tasks.html" lista
  putStrLn $ colorGreen ++ "Detyrat u eksportuan ne tasks.html!" ++ colorReset
  return lista

-- DSL EXECUTION
handleChoice "10" lista = do
  file <- prompt "Shkruaj emrin e file DSL:"
  lista' <- runDSLFile file lista
  putStrLn $ colorGreen ++ "Gjendja e detyrave pas DSL:" ++ colorReset
  printList lista'
  return lista'

-- DEFAULT
handleChoice _ lista = do
  putStrLn $ colorRed ++ "Zgjedhje e pasakte! Ju lutem provoni perseri." ++ colorReset
  return lista

------------------------------------------------------------
-- 1) SHTO DETYRE
------------------------------------------------------------

cliShtoDetyre :: TaskList -> IO TaskList
cliShtoDetyre lista = do
  idStr <- prompt "ID:"
  tit <- prompt "Titulli:"
  desc <- prompt "Pershkrimi:"
  prStr <- prompt "Prioriteti (Low/Medium/High):"
  dl <- prompt "Afati (ose Enter per asnje):"

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

------------------------------------------------------------
-- 2) HIQ DETYRE
------------------------------------------------------------

cliHiq :: TaskList -> IO TaskList
cliHiq lista = do
  idStr <- prompt "ID e detyres per heqje:"
  case parseId idStr of
    Nothing -> do
      putStrLn $ colorRed ++ "ID e pasakte!" ++ colorReset
      return lista
    Just idVal -> do
      putStrLn $ colorYellow ++ "Detyra u hoq (nese ekzistonte)." ++ colorReset
      return (hiqDetyre lista idVal)

------------------------------------------------------------
-- 3) NDRYSHO STATUS
------------------------------------------------------------

cliNdrysho :: TaskList -> IO TaskList
cliNdrysho lista = do
  idStr <- prompt "ID e detyres:"
  sStr <- prompt "Status (Pending/Completed):"

  case (parseId idStr, parseStatus sStr) of
    (Just idVal, Just statusVal) -> do
      putStrLn $ colorGreen ++ "Statusi u ndryshua!" ++ colorReset
      return (ndryshoStatusin lista idVal statusVal)
    _ -> do
      putStrLn $ colorRed ++ "Gabim ne input!" ++ colorReset
      return lista

------------------------------------------------------------
-- 4) FILTRA
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
    "4" ->
      printList (renditSipasPrioritetit lista)
    "0" -> return ()
    _ ->
      putStrLn $ colorRed ++ "Zgjedhje e pasakte!" ++ colorReset

------------------------------------------------------------
-- 5) RAPORTE
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
    _ -> putStrLn $ colorRed ++ "Zgjedhje e pasakte!" ++ colorReset