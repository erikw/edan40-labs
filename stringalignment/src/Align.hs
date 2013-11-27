module Align where


{----Similarity scoring----}
score ::  Int -> Int -> Int -> Char -> Char -> Int
score spaceSc _ _ '-' _ = spaceSc
score spaceSc _ _ _ '-' = spaceSc
score _ matchSc misSc x y
        | x == y = matchSc
        | otherwise = misSc


similarityScore :: (Char -> Char -> Int) -> String -> String -> Int
similarityScore scoreFcn [] y = sum $ map (scoreFcn '-') y
similarityScore scoreFcn x [] = sum $ map (scoreFcn '-') x
similarityScore scoreFcn (x:xs) (y:ys) = maximum [match, spaceUp, spaceDown]
                                where
                                match = scoreFcn x y + similarityScore scoreFcn xs ys
                                spaceUp = scoreFcn '-' y  + similarityScore scoreFcn (x:xs) ys
                                spaceDown = scoreFcn x '-' + similarityScore scoreFcn xs (y:ys)

{----Tests----}
score1 = score (-1) 0 (-1)
simTest1 = similarityScore score1 "writers" "vintner"
simCheck1 = simTest1 == (-5)

score2 = score (-2) 1 (-1)
simTest2 = similarityScore score2 "HASKELL" "PASCAL"
simCheck2 = simTest2 == (-2)
