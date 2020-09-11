{-Exercise 1 
Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
using the higher-order functions map and filter.
-}


mapFil :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFil f pred = map f. filter pred

{-Exercise 2
Without looking at the definitions from the standard prelude, define the
higher-order functions all, any, takeWhile, and dropWhile
-}

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p 

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []         = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

dropWhile' _ []         = []
dropWhile' p (x:xs) = if p x then takeWhile' p xs else x:xs 

--Guard notation
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x : xs


{-Exercise 3
Redefine the functions map f and filter p using foldr.
-}

map' f = foldr (\x xs -> f x : xs) []
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

{-Exercise 4
Using foldl, define a function dec2int :: [int] -> int that
converts a decimal number into an integer. 
> dec2int [2,3,4,5]
2345
-}
dec2int :: Num a => [a] -> a
dec2int = foldl(\x y -> (10 * x) + y) 0

id' :: a -> a
id' = \x -> x 

compose :: [a -> a] -> (a -> a)
compose = foldl (.) id'



{-Exercise 5 
Explain why the following definition is invalid:
sumsqreven = compose [sum, map (^2), filter even]

because it takes the first few values from the list, while you want those to be the last few.

-}

