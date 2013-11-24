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

==============Bass=====================

> type BassStyle = ([Int], Ratio Int)
> basicBass, calypsoBass, boogieBass :: BassStyle
> basicBass = ([0,4], hn)
> calypsoBass = ([-1, -1, 0, 2, -1, -1, 0, 2], en)
> boogieBass = ([0, 4, 5, 4, 0, 4, 5, 4], en)
>
>
> scPatterns :: [[Int]]
> scPatterns = [
>		[0, 2, 4, 5, 7, 9, 11],	-- (Major)
>		[0, 2, 4, 6, 7, 9, 11],
>		[0, 2, 4, 5, 7, 9, 10],
>		[0, 2, 3, 5, 7, 8, 10], -- (Minor)
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
> scaleIndex pitch key = fromJust $ elemIndex (mod pitch 12) key
>
> patternFromIndex :: Int -> ChordType -> [Int]
> patternFromIndex index cType = scPatterns !! scIndex
> 			where scIndex
> 				| cType == major = fst (scaleTable !! index)
> 				| otherwise = snd (scaleTable !! index)

> -- Get a scale pattern for a chord given a key.
> scalePattern :: Key -> Chord -> ChordType -> [Int]
> scalePattern key chord cType = patternFromIndex rootIndex cType
> 			where rootIndex = scaleIndex (head chord) key
>
> modround :: Int -> Int -> Int
> modround i chrd = mod (i+chrd) 12
>
> genBassChord :: Key ->  BassStyle -> (Chord, ChordType, Ratio Int) -> Chord
> genBassChord key (style,_) (chord, ctype, crtio) = map (pickNote modPattern) redStyle
>	 			where
>	 			pattern = scalePattern key chord ctype
>				modPattern = map (modround (head chord)) pattern
>	 			notesToPlay = (round $ (rtof crtio) * (float (length style)))
>	 			redStyle = take  notesToPlay style
>				oct =  snd (pitch $ head chord)
>				pickNote pttr e
>					| e == -1 = e
>					| otherwise = oct * 12 + (pttr !! e)
>
> type ChordType = [Int]
> major, minor :: ChordType
> major = [0, 2, 4]
> minor = [5, 0, 2]
>
> type ChordProgression = [(Chord, ChordType, Ratio Int)]
>
> genBassChordNote :: Key ->  BassStyle -> (Chord, ChordType, Ratio Int) -> Music
> genBassChordNote key style@(s, dur) bassChord =  foldr1 (:+:) [mkNote pi | pi <- chord]
> 					where
> 					chord = genBassChord key style bassChord
> 					mkNote pi
> 						| pi == -1 = Rest (dur)
> 						| otherwise = Note (pitch pi) (dur) [Volume 50]
>
>
> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass _ _ [] = c 0 0 [Volume 0]
> autoBass style key cprog = foldr1 (:+:) (map (genBassChordNote key style) cprog)
>

=============Voicing===============

> autoChord :: Key -> ChordProgression -> Music
> autoChord key cp = genChord key [] cp
>
> genChord :: Key -> Chord -> ChordProgression -> Music
> genChord _ _ [] = c 0 0 [Volume 0]
> genChord key prevChrd ((chord, chordType, dur):cps) =  chrdComp :+: genChord key nextChord cps
> 				where
> 				chrdComp = foldr1 (:=:) [Note (pitch pi) dur [Volume 50] | pi <- nextChord]
> 				nextChord = bestChord prevChrd fullChord
> 				fullChord = makeChord key chord chordType
>
> makeChord :: Key -> Chord -> ChordType -> Chord
> makeChord key chord cType = map ((+) (head chord)) (map ((!!) modPattern) cType)
> 			where
>			modPattern = map (modround (head chord)) scPattern
>	 		scPattern = scalePattern key chord cType
>
> bestChord :: Chord -> Chord -> Chord
> bestChord prevChrd chord = map limitPitch chord
>
> -- Move the pitch in the desired range of E4-G5
> limitPitch :: AbsPitch -> AbsPitch
> limitPitch pi 
> 	| pi < (pitchClass E + 4 * 12) = limitPitch (pi + 12)
> 	| pi > (pitchClass G + 5 * 12) = limitPitch (pi - 12)
> 	| otherwise = pi
>


\end{verbatim} }

