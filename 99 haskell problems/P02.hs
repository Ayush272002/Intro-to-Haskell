myButLast :: [a] -> a
myButLast xs
  | length xs < 2 = error "small"
  | otherwise = reverse xs !! 1