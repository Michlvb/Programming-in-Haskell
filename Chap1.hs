module Main where
import System.IO
import System.Environment 

-- ++ adds two lists
qsort :: Ord a => [a] -> [a]
qsort []    = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                 where
                     smaller = [a | a <- xs, a < x]
                     larger  = [b | b <- xs, b > x]

--Exercises 
-- 2. Show that sum [x] = x for any number of x
sum' :: Num p => [p] -> p
sum'    [] = 0
sum' (x : xs) = x + sum' xs

--3. Define a function product that produces the product of a list of numbers
product' :: Num p => [p] -> p
product' [] = 1
product' (x:xs) = x * product' xs

--4. Reverse quicksort

revQuicksort :: Ord a => [a] -> [a]
revQuickSort [] = []
revQuicksort (x:xs) = revQuicksort larger ++ [x] ++ revQuickSort smaller
                      where
                        larger  = [a | a <- xs, a > x]
                        smaller = [b | b <- xs, b <= x]

--5 What is the effect of replacing <= by < in the definition of sort
--The duplicate values will be discarded and it produces a sorted list with no duplicates.

