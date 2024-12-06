myLength :: [a] -> Int
myLength xs = foldl(\acc x -> acc + 1) 0 xs