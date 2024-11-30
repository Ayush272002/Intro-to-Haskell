{-
(*) Find the last-but-one (or second-last) element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y'
-}

myButLast :: [a] -> a
myButLast xs
  | length xs < 2 = error "list too short"
  | otherwise = head (tail (reverse xs))
