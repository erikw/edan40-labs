module Align where
import Data.List(elemIndices, intersperse)
import Data.Char(chr)

{----2 a)----}
{----Similarity scoring----}
score ::  Int -> Int -> Int -> Char -> Char -> Int
score spaceSc _ _ '-' _ = spaceSc
score spaceSc _ _ _ '-' = spaceSc
score _ matchSc misSc x y
        | x == y = matchSc
        | otherwise = misSc

type Scorer = (Char -> Char -> Int)

similarityScore :: Scorer -> String -> String -> Int
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

{----2 b)----}
-- Prepend h1 and h2 respectivly to each tuple's two lists.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

{----2 c)----}
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = map ((!!) xs) maxIndices
                    where
                    xsValues = map valueFcn xs
                    maxVal = maximum xsValues
                    maxIndices = elemIndices maxVal xsValues

{----Tests----}
maximaTest = maximaBy length ["cs", "efd", "lth", "it"]
maximaCheck = maximaTest == ["efd", "lth"]

{----2 d)----}
type AlignmentType = (String,String)

alignScore :: Scorer -> AlignmentType -> Int
alignScore scorer (str1, str2) = sum $ zipWith scorer str1 str2

optAlignments :: (AlignmentType -> Int) -> String -> String -> [AlignmentType]
{-optAlignments scoreFcn [] y = [(take (length y) (repeat '-'), y)]-}
{-optAlignments scoreFcn x [] = [(x, take (length x) (repeat '-'))]-}
optAlignments scoreFcn [] [] = [("", "")]
optAlignments scoreFcn [] (y:ys) = attachHeads '-' y (optAlignments scoreFcn [] ys)
optAlignments scoreFcn (x:xs) [] = attachHeads x '-' (optAlignments scoreFcn xs [])
optAlignments scoreFcn (x:xs) (y:ys) = maximaBy scoreFcn $ concat [match, spaceUp, spaceDown]
                                where
                                match = attachHeads x y (optAlignments scoreFcn xs ys)
                                spaceUp = attachHeads '-' y (optAlignments scoreFcn (x:xs) ys)
                                spaceDown = attachHeads x '-' (optAlignments scoreFcn xs (y:ys))


{----Tests----}
alignScoreFcn1 = alignScore score1
alignTest1 = optAlignments alignScoreFcn1 "writers" "vintner"
alignExpected1 = [("writ-ers","vintner-"), ("wri-t-ers","v-intner-"), ("wri-t-ers","-vintner-")]
alignCheck1 = alignTest1 == alignExpected1

{----2 e)----}
outputOptAlignments :: (AlignmentType -> Int) -> String -> String -> IO ()
outputOptAlignments scoreFcn str1 str2 = do
                                    putStrLn ("There are " ++  (show $ length aligns) ++ " optimal alignments:")
                                    putStrLn $ concat (map alignFmt aligns)
                                    where
                                    aligns = optAlignments scoreFcn str1 str2
                                    alignFmt :: AlignmentType -> String
                                    alignFmt (a0, a1) = "\n\n" ++ spaceIt a0 ++ "\n" ++ spaceIt a1 ++ "\n"
                                        where
                                        spaceIt :: String -> String
                                        spaceIt = intersperse ' '


{----Tests----}
outputTest1 = outputOptAlignments alignScoreFcn1 "writers" "vintner"

{----3----}
similarityScoreMemz :: Scorer -> String -> String -> Int
similarityScoreMemz scoreFcn xs ys = simScore (length xs) (length ys)
    where
    simScore i j = scoreTable !! i !! j
    scoreTable = [[scoreEntry i j | j <- [0..]] | i <- [0..]]

    scoreEntry :: Int -> Int -> Int
    scoreEntry 0 0 = 0
    scoreEntry 0 j = scoreFcn '-' (y j) + simScore 0 (j-1)
    scoreEntry i 0 = scoreFcn (x i) '-' + simScore (i-1) 0
    scoreEntry i j = maximum [match, spaceX, spaceY]
        where
        match = scoreFcn (x i) (y j) + simScore (i-1) (j-1)
        spaceX = scoreFcn '-' (y j) + simScore i (j-1)
        spaceY = scoreFcn (x i) '-' + simScore (i-1) j
    x i =  xs !! (length xs - i)
    y j =  ys !! (length ys - j)


optAlignmentsMemz :: Scorer -> String -> String -> [AlignmentType]
optAlignmentsMemz scoreFcn xs ys = snd $ optAlign (length xs) (length ys)
    where
    optAlign i j = alignTable !! i !! j
    alignTable = [[alignEntry i j | j <- [0..]] | i <- [0..]]

    alignEntry :: Int -> Int -> (Int, [AlignmentType])
    alignEntry 0 0 = (0, [("", "")])
    alignEntry 0 j = (newScore, newAligns)
            where
            (memScore, memAligns) = optAlign 0 (j-1)
            newAligns = attachHeads '-' (y j) memAligns
            newScore = scoreFcn '-' (y j) + memScore
    alignEntry i 0 = (newScore, newAligns)
            where
            (memScore, memAligns) = optAlign (i-1) 0
            newAligns = attachHeads (x i) '-' memAligns
            newScore = scoreFcn (x i) '-' + memScore
    alignEntry i j =  (maxScore, maxmemAligns)
            where
            maxElems = maximaBy fst [match, spaceX, spaceY]
            maxScore = (fst . head) maxElems
            maxmemAligns = concat $ map snd maxElems
            match = (newScore, newAligns)
                where
                (memScore, memAligns) = optAlign (i-1) (j-1)
                newAligns = attachHeads (x i) (y j) memAligns
                newScore = scoreFcn (x i) (y j) + memScore
            spaceX = (newScore, newAligns)
                where
                (memScore, memAligns) = optAlign i (j-1)
                newAligns = attachHeads '-' (y j) memAligns
                newScore = scoreFcn '-' (y j) + memScore
            spaceY = (newScore, newAligns)
                where
                (memScore, memAligns) = optAlign (i-1) j
                newAligns = attachHeads (x i) '-' memAligns
                newScore = scoreFcn (x i) '-' + memScore
    x i =  xs !! (length xs - i)
    y j =  ys !! (length ys - j)

{----Tests----}
simMemzTest1 = similarityScoreMemz score1 "writers" "vintner"
simMemzCheck1 = simMemzTest1 == (-5)

simMemzTest2 = similarityScoreMemz score2 "HASKELL" "PASCAL"
simMemzCheck2 = simMemzTest2 == (-2)


alignMemzTest1 = optAlignmentsMemz score1 "writers" "vintner"
alignMemzExpected1 = [("writ-ers","vintner-"), ("wri-t-ers","v-intner-"), ("wri-t-ers","-vintner-")]
alignMemzCheck1 = alignMemzTest1 == alignMemzExpected1

alignMemzTest2 = optAlignmentsMemz score1 "aferociousmonadatemyhamster" "functionalprogrammingrules"
