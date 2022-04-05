gap :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
gap g m n = search primes
  where
    isPrime :: Integer -> Bool
    isPrime x
      | null [a | a <- [2 .. x `div` 2], x `mod` a == 0] = True
      | otherwise = False

    primes = [a | a <- [m .. n], isPrime a]

    search (x : y : xs) = if y - x == g then Just (x, y) else search (y : xs)
    search _ = Nothing
