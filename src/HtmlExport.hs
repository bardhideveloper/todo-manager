module HtmlExport
  ( exportHtml
  ) where

import Types
import System.IO

------------------------------------------------------------
-- Ndihmës për të krijuar HTML
------------------------------------------------------------

htmlHeader :: String
htmlHeader =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \<head>\n\
  \  <meta charset=\"UTF-8\">\n\
  \  <title>Task List</title>\n\
  \  <style>\n\
  \    table { border-collapse: collapse; width: 100%; }\n\
  \    th, td { border: 1px solid #333; padding: 8px; text-align: left; }\n\
  \    th { background-color: #f2f2f2; }\n\
  \  </style>\n\
  \</head>\n\
  \<body>\n\
  \<h2>Task List</h2>\n\
  \<table>\n\
  \<tr>\n\
  \  <th>ID</th>\n\
  \  <th>Title</th>\n\
  \  <th>Description</th>\n\
  \  <th>Priority</th>\n\
  \  <th>Deadline</th>\n\
  \  <th>Status</th>\n\
  \</tr>\n"

htmlFooter :: String
htmlFooter =
  "</table>\n\
  \</body>\n\
  \</html>"

------------------------------------------------------------
-- Një Task → një rresht HTML
------------------------------------------------------------

taskToHtml :: Task -> String
taskToHtml t =
  "<tr>\n"
  ++ "<td>" ++ show (taskId t) ++ "</td>\n"
  ++ "<td>" ++ title t ++ "</td>\n"
  ++ "<td>" ++ description t ++ "</td>\n"
  ++ "<td>" ++ show (priority t) ++ "</td>\n"
  ++ "<td>" ++ show (deadline t) ++ "</td>\n"
  ++ "<td>" ++ show (status t) ++ "</td>\n"
  ++ "</tr>\n"

------------------------------------------------------------
-- Lista e detyrave → HTML komplet
------------------------------------------------------------

taskListToHtml :: TaskList -> String
taskListToHtml tasks =
  htmlHeader
  ++ concatMap taskToHtml tasks
  ++ htmlFooter

------------------------------------------------------------
-- Funksioni për eksportim në HTML
------------------------------------------------------------

exportHtml :: FilePath -> TaskList -> IO ()
exportHtml file tasks = do
  writeFile file (taskListToHtml tasks)
  putStrLn "U eksportuan ne HTML!"