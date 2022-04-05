import Data.Char (toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram str = (length . nub $ filter (`elem` ['a' .. 'z']) elements) == length ['a' .. 'z']
  where
    elements = map toLower str
