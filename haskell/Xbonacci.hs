xbonacci' :: Num a => [a] -> Int -> [a]
xbonacci' as n = as ++ next as (n - length as)
  where
    next xs i
      | i == 0 = []
      | otherwise = sum xs : next (tail xs ++ [sum xs]) (i - 1)


xbonacci :: Num a => [a] -> Int -> [a]
xbonacci as n
  | i == 0 = []
  | otherwise = 
