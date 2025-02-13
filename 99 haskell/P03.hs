{-
(*) Find the K'th element of a list.

The first element in the list is number 1. Example:

* (element-at '(a b c d e) 3)
c
Example in Haskell:

λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'
-}

elementAt :: (Num b, Enum b, Eq b) => [a] -> b -> a
elementAt xs n = head [x | (x, i) <- zip xs [1..], i == n]