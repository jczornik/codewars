countSheep :: Int -> String
countSheep n = concat $ take n $ map (\i -> show i ++ " sheep...") [1 ..]
