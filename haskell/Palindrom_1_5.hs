palindrome :: Int -> Int -> Maybe [Int]
palindrome x y
  | x < 0 || y < 0 = Nothing
  | y == 0 = Just []
  | otherwise = Just $ take y [z | z <- [x ..], isPalindrom z]
  where
    isPalindrom x'
      | x' < 10 = False
      | otherwise = read (reverse $ show x') == x'
