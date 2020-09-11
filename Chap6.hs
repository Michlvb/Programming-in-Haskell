{-Exercise 1
Define the exponentiation operator ^ for non-negative integers using
the same pattern of recursion as the multiplication operator *, and
show how 2 ^ 3 is evaluated using your definition.
Recall:
(*)   :: Int -> Int -> Int
m * 0   =  0
m * n = m + (m * n)
-}

exp' :: Int -> Int -> Int
exp' _ 0 = 1
exp' n e = n * exp' n (e-1)

{- 
= {applying exp'}
  2 * exp' 2 (3-1)
= {applying exp'}
  2 * (2 exp' 2 (2-1))
= {applying exp'}
  2 * (2 * (2 exp' 2 (1-1)))
= {applying exp'}
  2 * (2 * (2 * 1))
= {applying *}
    8 
-}

{-Exercise 2
Using the definitions given in this chapter, show how length [1,2,3],drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.


length:

= {applying length}
  1 + length [2,3]
= {applying length}
  1 + 1 + length [3]
= {applying length}
  1 + 1 + 1 + length []
= {applying length}
  1 + 1 + 1 + 0
= {applying +}
  3

Using n+k pattern
Drop:
= {applying drop}
  2 [2, 3, 4, 5]
= {applying drop}
  1 [3, 4, 5]
= {applying drop}
  0 [4, 5]
=
 [4,5]

Init:
= {applying int}
  1 : init [2,3]
= {applying init}
  1 : 2 : init [3]
= {applying init}
  1 : 2 : [] 
= 
 [1,2]
-}

{-Exercise 3
Without looking at the definitions from the standard prelude,
define the following library functions using recursion.

- Decide if all logical values in a list are True:
    and :: [Bool] -> Bool
- Concatenate a list of lists:
    concat :: [[a]] -> [a]
- Produce a list with n identical elements:
    replicate :: Int -> a -> [a]
- Select the nth element of a list:
    (!!) :: [a] -> Int -> a
- Decide if a value is an element of a list:
    elem :: Eq a => a -> [a] -> Bool
-}
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False = False
            | otherwise = and' xs
        
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

nth :: [a] -> Int -> a
nth (x:xs) n -- | null (x:xs) = "Error"
             | n == 0  = x
             | otherwise = nth xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys

{-Exercise 4
Define a recursive function merge :: Ord a => [a] -> [a] -> [a]
that merges two sorted lists to give a single sorted list. For exmaple:
> merge [2,5,6][1,3,4]
    [1,2,3,4,5,6]
-}
merge' :: Ord a => [a] -> [a] -> [a]
merge' xs []           = xs
merge' [] ys           = ys
merge' (x:xs) (y:ys) | x < y     = [x] ++ merge' xs (y:ys)
                     | otherwise = [y] ++ merge' (x:xs) ys 

{-Exercise 5 
Using merge, define a recursive function msort :: Ord a => [a] -> [a]
that implements merge sort, in which the empty list and singleton lists
are already sorted, and any other list is sorted by merging together the
two lists that result from sorting the two halves of the list separately
-}

halve :: [a] -> ([a], [a])
halve xs = (first, second)
        where first  = take (length xs `div` 2) xs
              second = drop (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge' (msort xss) (msort yss)
        where (xss, yss) = halve xs 

{-Exercise 6
Using the five-step process, define the library functions that calculate
the sum of a list of numbers, take a given number of elements from 
the start of a list, and select the last element of a non-empty list.-}

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = 1 + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _  = []
take' n (x:xs) = x : take' (n-1) xs

last' :: Ord a => [a] -> a
--last' [] = "error" 
last' [x] = x
last' (x:xs) = last' xs
