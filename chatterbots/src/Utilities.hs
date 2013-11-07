module Utilities where

-- Like normal map, but with two functions and two variables.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- The value of f(x) if x is not Nothing.
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Logical or selection of the two arguments.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Tries to apply function f to x and returns that value if succeeded. Else just x is returned.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Apply f to x until f(x) == x.
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)


-- Get the index of the element in the list at position u*length(list).
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
