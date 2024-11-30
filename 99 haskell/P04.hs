{-
(*) Find the number of elements in a list.
Example in Haskell:

λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13
-}

myLength :: [a] -> Int
myLength xs = foldr (\x acc -> acc + 1) 0 xs