module CLI.Colors
  ( colorReset,
    colorRed,
    colorGreen,
    colorYellow,
    colorBlue,
    colorCyan,
    colorWhite,
  )
where

colorReset :: String
colorReset = "\x1b[0m"

colorRed :: String
colorRed = "\x1b[31m"

colorGreen :: String
colorGreen = "\x1b[32m"

colorYellow :: String
colorYellow = "\x1b[33m"

colorBlue :: String
colorBlue = "\x1b[34m"

colorCyan :: String
colorCyan = "\x1b[36m"

colorWhite :: String
colorWhite = "\x1b[37m"
