{-
(**) Replicate the elements of a list a given number of times.
Example:

* (repli '(a b c) 3)
(A A A B B B C C C)
Example in Haskell:

λ> repli "abc" 3
"aaabbbccc"
-}

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> replicate' n x) xs