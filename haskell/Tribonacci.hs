tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci _ 0 = []
tribonacci (a, _, _) 1 = [a]
tribonacci (a, b, _) 2 = [a, b]
tribonacci (a, b, c) n = result
  where
    result = a : b : c : nextValues (a, b, c) (n - 3)
    nextValues _ 0 = []
    nextValues (x, y, z) n = x + y + z : nextValues (y, z, x + y + z) (n - 1)
