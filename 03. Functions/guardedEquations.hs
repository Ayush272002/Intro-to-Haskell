myAbs :: Int -> Int
myAbs n
  | n >= 0 = n
  | otherwise = -n

howMuchDoYouLikeHasskell2 :: Int -> String
howMuchDoYouLikeHasskell2 x
  | x < 3 = "I dislike it !!"
  | x < 7 = "It's ok!"
  | otherwise = "I love it!"