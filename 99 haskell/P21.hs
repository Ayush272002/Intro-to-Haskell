{-
Insert an element at a given position into a list.

Example:

\* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
Example in Haskell:

λ> insertAt 'X' "abcd" 2
"aXbcd"
-}

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n - 1) xs ++ [x] ++ drop (n - 1) xs