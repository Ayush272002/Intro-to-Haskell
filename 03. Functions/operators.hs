square :: Int -> Int
square = (^ 2)

reci :: (Fractional a) => a -> a
reci = (1 /)

square' :: Int -> Int
square' = \x -> x ^ 2

reci' :: (Fractional a) => a -> a
reci' = \x -> 1 / x