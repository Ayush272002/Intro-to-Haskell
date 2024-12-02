{-
(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples:

\* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

\* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)
Examples in Haskell:

λ> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"

λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
-}

rotate :: [a] -> Int -> [a]
rotate xs n
  | n >= 0 = drop k xs ++ take k xs
  | otherwise = drop k xs ++ take k xs
  where
    len = length xs
    k = (n `mod` len + len) `mod` len