import Data.Char 

{-Exercise 1
Using a list comprehension, give an expression that calculates the sum
1^2 + 2^2 + ... 100^2 of the first one hundred integer squares-}

squares :: Integer -> Integer
squares x = x * x

listOfFirst100Squares' = sum[squares x| x <- [1..100]]

listOfFirst100Squares = sum[(x^2)| x <- [1..100]]
{-Exercise 2
In a similar way to the function length, show how the library function replicate :: Int -> a -> [a]
that produces a list of identical elements can be defined using a list comprehension

>replicate 3 True
[True, True, True]
-}
replicateCom n val = [val | _ <- [0..n-1]]

{-Exercise 3
A triple (x,y,z) of positive integers is pythagorean if x^2 + y^2 = z^2.
Using a list comprehension, define a function pyths :: Int -> [(Int, Int, Int)]
that returns the list of all pythagorean triples
whose components are at most a given limit.

> pyths 10
[(3,4,5), (4,3,5), (6,8,10), (8,6,10)]
-}
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z)| x <- [1..n], y <- [1..n], z <-[1..n], x^2+y^2 == z^2]

{-Exercise 4
A positive integer is perfect if it equals the sum of its factors, excluding the number itself.
Using a list comprehension and the function factors, define a function
perfects :: Int -> [Int] that returns the list of all perfect numbers
up to a given limit. 
> perfects 500
[6, 28, 496] -}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect' :: Int -> [Int]
perfect' n = [x | x <- [1..n], (sum(factors x))-x == x]

{-Exercise 5
Show how the single comprehension [(x,y) | x <- [1,2,3], y <- [4,5,6]]
with two generators can be re-expressed using 
two comprehensions with single generators.
Hint: Make use of the library function concat 
      and nest one comprehension within the other
-}

single1 = [x | x <- [1,2,3]]
single2 = [y | y <- [4,5,6]]
added = [single1,single2]
new1 = concat added

{-Exercise 6 
Redefine the function positions using the function find :: (a -> Bool) -> [a] -> Maybe a).
Positions  	:: Eq a => a -> [a] -> [int]
Positions x xs 	= [i | (x’, i) <- zip xs [0..n], x == x’]
			    where n = length xs -1
-}

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t,k == k']

rePositions :: Eq a => a -> [a] -> [Int]
rePositions x xs = find x (zip xs [0..n])
                    where n = length xs -1 

{-Exercise 7
The scalar product of two lists of integers xs and ys of length n is given
by the sum of the products of corresponding integers: 
n-1 
∑  (xsi * ysi)
i=0
In a similar manner to the function chisqr, show how a list comprehension 
can be used to define a function scalarproduct :: [Int] -> [Int] -> Int
that returns the scalar product of two lists. For example:
> scalarproduct [1,2,3][4,5,6]
    32
-}

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum[(x*y) | (x,y) <- zip xs ys] 

{-Exercise 8
Modify the Caesar cipher program to also handle upper-case letters.
-}


let2int  :: Char -> Int
let2int c = if isLower c then ord c - ord 'a' else ord c - ord 'A'
          

int2let :: Bool -> Int -> Char
int2let bool n = if bool then chr (ord 'a' + n) else chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isAlpha c = int2let(isLower c )((let2int c + n) `mod` 26)
          | otherwise = c