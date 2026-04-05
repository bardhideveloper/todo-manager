module CLI.Colors
  ( colorReset
  , colorRed
  , colorGreen
  , colorYellow
  , colorBlue
  , colorCyan
  , colorWhite
  ) where

colorReset = "\x1b[0m"
colorRed   = "\x1b[31m"
colorGreen = "\x1b[32m"
colorYellow= "\x1b[33m"
colorBlue  = "\x1b[34m"
colorCyan  = "\x1b[36m"
colorWhite = "\x1b[37m"