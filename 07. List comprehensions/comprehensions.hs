-- {x^2 | x ∈ {1...5}}

comprehension1 :: (Num a) => [a] -> [a]
comprehension1 xs = [x ^ 2 | x <- xs]

myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- guards
evenList :: (Integral a) => [a] -> [a]
evenList xs = [x | x <- xs, even x]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

-- zip function
zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = [(x, y) | (x, y) <- zip xs ys]

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and [x <= y | (x, y) <- zip xs (tail xs)]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']