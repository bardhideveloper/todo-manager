module Main where

import Filters
import Persistence
import Reports
import System.IO (hFlush, stdout)
import TaskOperations
import Types
import Validation
import JsonExport

------------------------------------------------------------
-- Input helper
------------------------------------------------------------

prompt :: String -> IO String
prompt text = do
  putStr text
  putStr " "
  hFlush stdout
  getLine

------------------------------------------------------------
-- Printimi i detyrave
------------------------------------------------------------

printTask :: Task -> IO ()
printTask t = do
  putStrLn ("ID: " ++ show (taskId t))
  putStrLn ("Titulli: " ++ title t)
  putStrLn ("Përshkrimi: " ++ description t)
  putStrLn ("Prioriteti: " ++ show (priority t))
  putStrLn ("Afati: " ++ show (deadline t))
  putStrLn ("Statusi: " ++ show (status t))
  putStrLn "-------------------------"

printList :: TaskList -> IO ()
printList lista =
  mapM_ printTask lista

------------------------------------------------------------
-- Menu
------------------------------------------------------------

menu :: TaskList -> IO ()
menu lista = do
  putStrLn "\n=== To-Do Manager ==="
  putStrLn "1. Shto detyre"
  putStrLn "2. Hiq detyre"
  putStrLn "3. Ndrysho statusin"
  putStrLn "4. Shfaq te gjitha detyrat"
  putStrLn "5. Filtra & Kerkime"
  putStrLn "6. Raporte"
  putStrLn "7. Eksporto detyrat ne JSON"
  putStrLn "8. Importo detyrat nga JSON"
  putStrLn "0. Dil"

  choice <- prompt "Zgjedhja:"
  handleChoice choice lista

handleChoice :: String -> TaskList -> IO ()
handleChoice "1" lista = do
  lista' <- cliShtoDetyre lista
  menu lista'
handleChoice "2" lista = do
  lista' <- cliHiq lista
  menu lista'
handleChoice "3" lista = do
  lista' <- cliNdrysho lista
  menu lista'
handleChoice "4" lista = do
  printList lista
  menu lista
handleChoice "5" lista = do
  cliFiltra lista
  menu lista
handleChoice "6" lista = do
  cliRaporte lista
  menu lista
handleChoice "7" lista = do
    exportToJson "tasks.json" lista
    menu lista
handleChoice "8" lista = do
    lista' <- importFromJson "tasks.json"
    menu lista'

-- Dalje + ruajtje
handleChoice "0" lista = do
  saveTasks "tasks.db" lista
  putStrLn "Detyrat u ruajten. Dalje..."
  return ()
handleChoice _ lista = do
  putStrLn "Zgjedhje e pasakte!"
  menu lista

------------------------------------------------------------
-- SHTO DETYRË (me validim)
------------------------------------------------------------

cliShtoDetyre :: TaskList -> IO TaskList
cliShtoDetyre lista = do
  idStr <- prompt "ID:"
  tit <- prompt "Titulli:"
  desc <- prompt "Pershkrimi:"
  prStr <- prompt "Prioriteti (Low/Medium/High):"
  dl <- prompt "Afati (ose Enter):"

  let maybeId = parseId idStr
  let maybePr = parsePriority prStr

  case (maybeId, maybePr) of
    (Just idVal, Just prVal) -> do
      let deadlineVal = if dl == "" then Nothing else Just dl
      let t = Task idVal tit desc prVal deadlineVal Pending
      putStrLn "Detyra u shtua!"
      return (shtoDetyre lista t)
    _ -> do
      putStrLn "Gabim ne input! ID duhet te jete numer dhe prioriteti i sakte."
      return lista

------------------------------------------------------------
-- HIQ DETYRË (me validim)
------------------------------------------------------------

cliHiq :: TaskList -> IO TaskList
cliHiq lista = do
  idStr <- prompt "ID e detyres per heqje:"

  case parseId idStr of
    Nothing -> do
      putStrLn "ID e pasakte!"
      return lista
    Just idVal -> do
      let lista' = hiqDetyre lista idVal
      putStrLn "Detyra u hoq (nese ekzistonte)."
      return lista'

------------------------------------------------------------
-- NDRYSHO STATUS (me validim)
------------------------------------------------------------

cliNdrysho :: TaskList -> IO TaskList
cliNdrysho lista = do
  idStr <- prompt "ID e detyres:"
  sStr <- prompt "Status i ri (Pending/Completed):"

  let maybeId = parseId idStr
  let maybeStatus = parseStatus sStr

  case (maybeId, maybeStatus) of
    (Just idVal, Just stVal) -> do
      let lista' = ndryshoStatusin lista idVal stVal
      putStrLn "Statusi u ndryshua!"
      return lista'
    _ -> do
      putStrLn "Gabim ne input! Kontrolloni ID dhe statusin."
      return lista

------------------------------------------------------------
-- FILTRA & KËRKIME
------------------------------------------------------------

cliFiltra :: TaskList -> IO ()
cliFiltra lista = do
  putStrLn "\n--- Filtra & Kerkime ---"
  putStrLn "1. Filtra sipas prioritetit"
  putStrLn "2. Filtra sipas statusit"
  putStrLn "3. Kerkim me fjale"
  putStrLn "4. Rendit sipas prioritetit"
  putStrLn "0. Kthehu"

  choice <- prompt "Zgjedhja:"

  case choice of
    "1" -> do
      prStr <- prompt "Prioriteti (Low/Medium/High):"
      case parsePriority prStr of
        Just prVal -> printList (filtroSipasPrioritetit prVal lista)
        Nothing -> putStrLn "Prioritet i pasakte!"
    "2" -> do
      stStr <- prompt "Statusi (Pending/Completed):"
      case parseStatus stStr of
        Just stVal -> printList (filtroSipasStatusit stVal lista)
        Nothing -> putStrLn "Status i pasakte!"
    "3" -> do
      k <- prompt "Fjala per kerkimin:"
      printList (kerkoDetyre k lista)
    "4" -> do
      printList (renditSipasPrioritetit lista)
    "0" -> return ()
    _ -> putStrLn "Zgjedhje e pasakte!"

------------------------------------------------------------
-- RAPORTE
------------------------------------------------------------

cliRaporte :: TaskList -> IO ()
cliRaporte lista = do
  putStrLn "\n--- Raporte ---"
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
    _ -> putStrLn "Zgjedhje e pasakte!"

------------------------------------------------------------
-- MAIN: Ngarkimi fillestar nga file
------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Sistemi u startua!"
  lista <- loadTasks "tasks.db"
  menu lista