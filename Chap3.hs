{-Exercise 1:
What are the types of the following values?
[‘a’, ‘b’, ’c’] -> [Char]
(‘a’, ‘b’, ’c’) -> (Char)
[(False, ‘O’), (True, ‘1’)] -> [(Bool, Char), (Bool, Char)]
([False, True], [‘0’, ‘1’, ‘2’]) -> ([Bool], [Char])
[tail, reverse, init] -> [[a] -> [a]]

Exercise 2:
What are the types of the following functions?
Second xs 	= head (tail xs)		== :: [a] -> a
Swap (x, y)	= (y,x)				== ::  (a, b) -> (b, a) or (b, a) -> (a, b)
Pair x y		= (x,y)				== :: a -> b -> (a, b)
Double x		= x *2				== :: Num a => a -> a
Palindrome xs = reverse xs == xs		== :: Eq a => [a] -> Bool  
Twice f x.  	= f (f x)				== :: (a -> a) -> a -> a. Prelude (t -> t) -> t -> t
									
Exercise 4:
Why is it not feasible in general for function types to be instance of the Eq class.
In general it isn’t feasible to have functions of the Eq class, 
because a lot of functions differ from types, arguments and results and in general you can already conclude that the functions aren’t equal in such cases.
-}