{-
(*) Reverse a list.
Example in Haskell:

λ> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
λ> myReverse [1,2,3,4]
[4,3,2,1]
-}

myReverse :: [a] -> [a]
myReverse xs = foldl (\acc x -> x : acc) [] xs

myReverse' :: [a] -> [a]
myReverse' xs = foldr (\x acc -> acc ++ [x]) [] xs