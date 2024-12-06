compress :: (Eq a) => [a] -> [a]
compress = foldl (\acc x -> if null acc || last acc /= x then acc ++ [x] else acc) []
