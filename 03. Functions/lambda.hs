double :: Int -> Int
double x = 2 * x

mult :: Int -> Int -> Int
mult x y = x * y

-- The same functions can be written using lambda expressions:
double' :: Int -> Int
double' = \x -> 2 * x

mult' :: Int -> Int -> Int
mult' = \x y -> x * y

mult'' :: Int -> Int -> Int
mult'' = \x -> (\y -> x * y)

alwaysZero :: Bool -> Int
alwaysZero = \_ -> 0

-- functions can be passed as arguments to other functions
apply :: (a -> b) -> a -> b
apply f x = f x