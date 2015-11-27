module Utilities where

-- Takes a tuple of functions f1 and f2, and a tuple of 
-- values x1 and x2 and returns a tuple of values where
-- f1 has been applied to x1 and f2 to x2
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Applies a function to a Just value and incaplsulates it inside Just 
-- before return. 
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Takes two Maybe values and returns one. If one of the values is Nothing
-- returns the other.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- If f(x) returns nothing, then return f(x). Else return x.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- If f changes the value of x, then reapply f to x.
-- Ex: fix (/2) <Even number> results in 0.0
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Select the first element in the list that is > the r:th percentile mark
-- Ex: pick 0.5 [1..10] == 0.6
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

