perimeter :: Integer -> Integer
perimeter = sum . map (* 4) . squares
  where
    squares x = go x 1 1
      where
        go 0 _ _ = [1]
        go 1 _ _ = [1, 1]
        go i a b = (a + b) : go (i - 1) b (a + b)
