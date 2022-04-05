import Debug.Trace

decompose :: Integer -> Maybe [Integer]
decompose x = reverse <$> f (x - 1) (x ^ 2)
  where
    f :: Integer -> Integer -> Maybe [Integer]
    f n rest
      | trace ("n: " ++ show n ++ " rest: " ++ show rest) False = undefined
    f 0 rest
      | rest == 0 = Just []
      | otherwise = Nothing
    f n rest
      | n ^ 2 == rest = Just [n]
      | n ^ 2 < rest = case f (n - 1) (rest - n ^ 2) of
        Just xs -> Just (n : xs)
        Nothing -> f (n - 1) rest
      | otherwise = f (floor $ sqrt $ fromInteger rest) rest
