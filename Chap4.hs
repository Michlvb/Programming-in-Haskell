--Exercise 1
halve'  :: [a] -> ([a], [a])
halve' [] = ([], [])
halve' xs =  splitAt ( length xs `div` 2) xs

--Exercise 2
--Define safetail using: (it handles the case when a null value is past or empty list)
-- A conditional expression;
-- B guarded equations;
-- C pattern matching.

safetail :: [a] -> [a]
safetail xs = if length xs == 0 then [] else tail xs 

safetail2 :: [a] -> [a]
safetail2 xs     | null xs = []
                 | otherwise = xs

safetail4 :: [a] -> [a] 
safetail4(_:xs) = xs
safetail4 _ = []

--Exercise 3
--Show how the logical disjunction operator V can be defined in four different ways
--Using pattern matching

-- (⋁)     :: Bool -> Bool -> Bool
-- True  ⋁ True  = True
-- True  ⋁ False = True
-- False ⋁ True  = True
-- False ⋁ False = False

or'   :: Bool -> Bool -> Bool
or' True  True   = True
or' True  False  = True
or' False True   = True
or' False False  = False  

or2' :: Bool -> Bool -> Bool
or2' True _ = True
or2' False value = value

or3' :: Bool -> Bool -> Bool
or3' False False = False
or3' _ _ = True

or4' :: Bool -> Bool -> Bool
or4' a b | b /= a   = True
         | otherwise = b 
         

{-Exercise 4
Redefine the following version of the conjuction 
operator using conditional expressions rather than pattern matching:

True ⋀ True	= True
_ ⋀ _ 		= False
-}

and'    :: Bool -> Bool -> Bool
and' x y = if x == True then 
              if y == True then True 
              else False
            else False

{-Exercise 5
Do the same for following version.
True  ⋀ b	    = True
False ⋀ _ 		= False 
-}
and2'   :: Bool -> Bool -> Bool
and2' x y = if x == True then y else 
            if x == False then x else False
            --unneeded if statement, first else could be just False.

{- Exercise 6
Show how the curried function definition mult x y z = x * y * z can
be understood in terms of lambda expressions.
-}
mult = \x -> \y -> \z -> x * y * z
        -- \x -> (\y -> (\z -> x * y * z))