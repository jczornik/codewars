uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder = foldl (\acc x -> if last acc == x then acc else acc ++ [x]) []
