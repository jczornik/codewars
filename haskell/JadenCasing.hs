import Data.Char (toUpper)

toJadenCase :: String -> String
toJadenCase = unwords . map (\(x : xs) -> toUpper x : xs) . words
