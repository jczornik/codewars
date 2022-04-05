humanReadable :: Int -> String
humanReadable x = showFormated h ++ ":" ++ showFormated m ++ ":" ++ showFormated s
  where
    h = x `div` 3600
    m = x `rem` 3600 `div` 60
    s = x `rem` 60

    format :: String -> String
    format [x] = '0' : [x]
    format x = x

    showFormated = format . show
