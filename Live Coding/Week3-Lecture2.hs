--
--  Week3-Lecture2.hs - Recursive and Higher Order Functions
--
import Data.Char

-- Recursive Functions
-- fac 5 = 5 * (4 * 3 * 2 * 1)
-- fac 5 = 5 * (fac 4)
--       = 5 * (4 * fac 3)
--       = 5 * (4 * (3 * fac 2))
--       = 5 * (4 * (3 * (2 * (fac 1))))
--       = 5 * (4 * (3 * (2 * (1 * fac 0))))
--       = 5 * (4 * (3 * (2 * (1 * 1))))
-- fac (-1) = -1 * (fac (-2))
--          = -1 * (-2 * fac (-3))
--          = ...
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

facSafe :: Int -> Int
facSafe n
  | n <= 0 = 1
  | otherwise = n * facSafe (n - 1)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 _ = []
take' n (x : xs) = x : take' (n - 1) xs

even' :: Int -> Bool
even' n = n `mod` 2 == 0

odd' :: Int -> Bool
odd' n = not (even' n)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort bigger
  where
    smaller = [a | a <- xs, a <= x]
    bigger = [a | a <- xs, a > x]

anything :: a
anything = anything

-- Higher Order Functions

-- it applies the fucntion twice like f(f(x))
twice :: (a -> a) -> a -> a
twice f x = f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = (f x) : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x : xs) = x * product' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

-- using foldr
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' op init [] = init
foldr' op init (x : xs) = x `op` (foldr' op init xs)

sum'' :: (Num a) => [a] -> a
sum'' = foldr (+) 0

product'' :: (Num a) => [a] -> a
product'' = foldr (*) 1

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

-- 1 : 2 : 3 : 4 : []
--
-- foldr (**) i  = 1 ** (2 ** (3 ** (4 ** i)))
--

-- foldr' :: (a -> b -> b) -> b -> [a] -> b

-- In the type type of foldr
--
--    a = type of list elements
--    b = type of "accumulator"
--

-- for eaxmple, in this fuction
--
--    a = a
--    b = [a]
--

duplicate :: [a] -> [a]
duplicate xs = foldr (\x acc -> x : x : acc) [] xs

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g = \x -> f (g x)

apply :: (a -> b) -> a -> b
apply f x = f x
