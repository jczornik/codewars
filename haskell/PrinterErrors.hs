printerError :: [Char] -> [Char]
printerError s = show (length errors) ++ "/" ++ show (length s)
  where
    correctColors = ['a' .. 'm']
    errors = filter (`notElem` correctColors) s
