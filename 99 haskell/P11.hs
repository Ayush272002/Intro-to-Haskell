{-
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:

\* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:

λ> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data ListItem a = Single a | Multiple Int a deriving (Show)

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

encodedModified :: (Eq a) => [a] -> [ListItem a]
encodedModified = map helper . encode
  where
    helper (1, x) = Single x
    helper (n, x) = Multiple n x