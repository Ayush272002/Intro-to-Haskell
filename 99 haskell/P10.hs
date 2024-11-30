{-
(*) Run-length encoding of a list.

Use the result of Problem 9 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:

\* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
Example in Haskell:

λ> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

pack :: (Eq a) => [a] -> [[a]]
pack xs =
  foldl
    ( \acc x ->
        if not (null acc) && last (last acc) == x
          then init acc ++ [last acc ++ [x]]
          else acc ++ [[x]]
    )
    []
    xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = helper (pack xs)
  where
    helper :: [[a]] -> [(Int, a)]
    helper [] = []
    helper xs = map (\sublist -> (length sublist, head sublist)) xs