\section{Auto generation of songs}
\label{autocomp}

{\small\begin{verbatim} 

> module AutoComp where
> import Data.Ratio
> import Data.List(elemIndex)
> import Data.Maybe(fromJust)
>
> import Haskore hiding(Key)
> import Control.Exception hiding (assert)
> 
>
>
>
>
> type BassStyle = ([Int], Ratio Int)
> basicBass, calypsoBass, boogieBass :: BassStyle
> basicBass = ([0,4], 1%2)
> calypsoBass = ([-1, -1, 0, 2, -1, -1, 0, 2], 1%8)
> boogieBass = ([0, 4, 5, 4, 0, 4, 5, 4], 1%8)
>
>
> scPatterns :: [[Int]]
> scPatterns = [
>		[0, 2, 4, 5, 7, 9, 11], 		-- (Major)
>		[0, 2, 4, 6, 7, 9, 11], 
>		[0, 2, 4, 5, 7, 9, 10], 
>		[0, 2, 3, 5, 7, 8, 10], 		-- (Minor)
>		[0, 2, 3, 5, 7, 9, 10],
>		[0, 1, 3, 5, 7, 8, 10]
>		]
>
> -- Table mapping a key scale position to a scale pattern. (major, minor). -1 = Null.
> scaleTable :: [(Int, Int)]
> scaleTable = [(0,-1), (2,4), (-1,5), (1,-1), (2,-1), (-1, 3), (-1, -1)]
> 
>
> type Chord = [AbsPitch]
> type Key = Chord
>
> -- 
> cMajor =  [0, 2, 4, 5, 7, 9, 11] :: Chord
>
> -- Print the pitches in a chord as characters.
> printChord :: Chord -> IO ()
> printChord aps = do
>			print $ map (fst . pitch) aps
>
> -- Get the index of a given pitch in a key.
> scaleIndex :: AbsPitch -> Key -> Int
> scaleIndex pitch key = fromJust $ elemIndex pitch key
>
> patternFromIndex :: Int -> [Int]
> patternFromIndex index = scPatterns !! scIndex
> 			where scIndex = fst (scaleTable !! index) -- TODO how to pick between major/minor?

> -- Get a scale pattern for a chord given a key.  
> scalePattern :: Key -> Chord -> [Int]
> scalePattern key chord = patternFromIndex rootIndex
> 			where rootIndex = scaleIndex (head chord) key
> 				
> modround :: Int -> Int -> Int
> modround i chrd = mod (i+chrd) 12
>
> genBassChord :: Key ->  BassStyle -> (Chord, Ratio Int) -> Chord
> genBassChord key (style,_) (chord, crtio) = map (work modPattern) redStyle
>	 			where 
>	 			pattern = scalePattern key chord
>	 			redStyle = take (round $ (rtof crtio) * (float (length style))) style
>				modPattern = (map (modround (head chord)) pattern)
>				work pttr e 
>					| e == -1 = e
>					| otherwise = pttr !! e
>
> genBassTest = genBassChord cMajor basicBass ([7, 11, 2], 1%1)
> genBassCheck = genBassTest == [7, 2]

> type ChordProgression = [(Chord, Ratio Int)]
>
> genBassChordNote :: Key ->  BassStyle -> (Chord, Ratio Int) -> Music
> genBassChordNote key style@(s, dur) bassChord =  foldr1 (:+:) [if pi == -1 then Rest (dur) else Note (pitch pi) (dur) [Volume 100] | pi <- chord]
> 					where chord = genBassChord key style bassChord
>
> -- Broken test.
>{-> calypsoProgress  = [(C, 1), (F, 1%2), (C, 1%2)]-}
>{-> genBassChordNoteTest = genBassChordNote cMajor calypsoBass calypsoProgress-}
>{-> genBassChordNoteCheck = genBassTest == [Rest (1%8), Rest (1%8), Note 0 (1%8) [Volume 100], Note 4 (1%8) [Volume 100], Rest (1%8), Rest (1%8),Note 0 (1%8) [Volume 100], Note 4 (1%8) [Volume 100]]-}
>
> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass style key cprog = foldr1 (:+:) (map (genBassChordNote key style) cprog)
>
>
> twinklePart1  = [(C, 1), (F, 1%2), (C, 1%2), (G, 1%2), (C, 1%2), (G, 1%2), (C, 1%2)] -- TODO generate the whole cord from the chord root given.
> twinklePart2  = [(C, 1%2), (G, 1%2),(C, 1%2), (G, 1%2), (C, 1%2), (G, 1%2),(C, 1%2), (G, 1%2)]
> twinkleComp = twinklePart1 ++ twinklePart2 ++ twinklePart1
> twinkleProgression = [([pitchClass pi], dur) | (pi,dur) <- twinkleComp]
> twinkleBass = autoBass basicBass cMajor twinkleProgression 
>
> twinkle = Instr "piano" (Tempo 3 (Phrase [Dyn SF] twinkleBass))

\end{verbatim} }

