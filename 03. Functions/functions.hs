removeLast :: [a] -> [a]
removeLast xs = reverse (tail (reverse xs))

removeElem :: Int -> [a] -> [a]
removeElem n xs = removeLast (take n xs) ++ drop n xs

abs' :: Integer -> Integer
abs' n = if n >= 0 then n else -n

howMuchDoYouLikeHaskell :: Int -> String
howMuchDoYouLikeHaskell x =
  if x < 3
    then "I dislike it!"
    else
      if x < 5
        then "It's okay."
        else
          "I love it!"