{-
(*) Duplicate the elements of a list.

Example:

\* (dupli '(a b c c d))
(A A B B C C C C D D)
Example in Haskell:

λ> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}

dupli :: [a] -> [a]
dupli xs = foldl (\acc x -> acc ++ [x] ++ [x]) [] xs