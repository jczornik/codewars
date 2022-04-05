findNextSquare :: Integer -> Integer
findNextSquare x = if isPerfect then (x' + 1) ^ 2 else -1
  where
    x' = floor $ sqrt $ fromIntegral x
    isPerfect
      | x == x' * x' = True
      | otherwise = False
