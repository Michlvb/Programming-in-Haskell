module Main where
import System.IO
import System.Environment 

--Exercise 1
{- Parenthesise the following arithmetic expressions:
    (2^3) * 4
    (2* 3) + (4 * 5)
    2 + (3 *( 4 ^ 5))
-}

--Exercise 2
-- Work through the examples from this chapter using Hugs.

--Exercise 3
{- Correct 3 errors in the following script and check if it works properly
N = a 'div' length xs
        where
	   a = 10
	 xs  = [1,2,3,4,5]

1. Div should be with back quotes `div`.
2. N isn’t allowed(capital letters aren't allowed at the start), so n instead.
3. Should be:
	n = a `div` length xs
	   where
		a = 10
		xs = [1,2,3,4,5]
-}
--Exercise 4
lastElem xs = head (reverse xs)
another xs = xs !! (length xs -1)
--Exercise 5
init' xs = take (length xs -1) xs
