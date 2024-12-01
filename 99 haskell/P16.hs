{-
(**) Drop every N'th element from a list.
 

Example:

* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
Example in Haskell:

λ> dropEvery "abcdefghik" 3
"abdeghk"
-}

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (x, i) <- zip xs [1..], i `mod` n /= 0]