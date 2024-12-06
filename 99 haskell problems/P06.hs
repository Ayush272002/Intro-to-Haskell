isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = rev xs == xs
  where
    rev :: [a] -> [a]
    rev xs = foldr (\x acc -> acc ++ [x]) [] xs