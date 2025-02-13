{-
(**) Pack consecutive duplicates of list elements into sublists.

If a list contains repeated elements they should be placed in separate sublists.

Example:

\* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))
Example in Haskell:

λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
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