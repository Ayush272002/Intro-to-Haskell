elementAt :: [a] -> Int -> a
elementAt as n
    | n < 0 = error "no negative number allowed"
    | length as < n = error "small"
    | otherwise = as !! (n-1)