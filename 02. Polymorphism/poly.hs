odd' :: Int -> Bool
odd' n = mod n 2 == 1

even' :: Int -> Bool
even' n = mod n 2 == 0

isUpper' :: Char -> Bool
isUpper' c = c >= 'A' && c <= 'Z'

isLower' :: Char -> Bool
isLower' c = c >= 'a' && c <= 'z'