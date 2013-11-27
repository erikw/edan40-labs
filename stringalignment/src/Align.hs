module Align where

space :: Char
space = '-'

score (Eq a):: a -> a -> Int
score space _ = -1
score _ space = -1
score x y 
        | x ==y = 0
        | otherwise = -1

similarityScore :: String -> String -> Int
similarityScore (x:xs) (y:ys) = maximum [match, spaceUp, spaceDown]
                                where
                                match = score(x,y) + similarityScore(xs, ys)
                                spaceUp = score(space,y) + similarityScore(x:xs, ys)
                                spaceDown = score(x,space) + similarityScore(xs, y:ys)
