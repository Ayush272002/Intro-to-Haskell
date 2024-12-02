{-
Create a list containing all integers within a given range.

Example:

\* (range 4 9)
(4 5 6 7 8 9)
Example in Haskell:

λ> range 4 9
[4,5,6,7,8,9]
-}

range :: Int -> Int -> [Int]
range l r = [l..r]