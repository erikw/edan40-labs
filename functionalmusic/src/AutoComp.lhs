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
>{-> data BassStyle = Basic | Calypso | Boogie  deriving (Eq, Show)-}
>{-> getBassStylePatter :: BassStyle -> [Int]-}
>{-> getBassStylePatter (Basic bs) = [1, 5]-}
> -- 0 means silence.
>
>{-> type ChordProgression = [[Chord]]-}
>
>{->-}
>{-> autoBass :: BassStyle -> Key -> ChordProgression -> Music  --- TODO select scale based on key.-}
>{-> autoBass (Basic bs) k cp = foldr1 :+: (map (genBassBar k (getBassStylePatter bs)) cp)-}
>{->-}
>{-> genBassBar :: [Chord] -> Key -> [Int] -> Music-}
>{-> genBassBar chrds key bsPatt = foldr1 :+: (map (Note (genPitch (scalePattern Key (first (first chrds)))) (1%2) bsPatt))-}
>{->-}
>{-> ionian :: [Int] -}
>{-> ionian  = [0, 2, 4, 5, 7, 9, 11]-}
>{-> scalePattern :: Key -> AbsPitch-}
>{-> scalePattern key pitch =  -}
>
>
>
>{-> genPitch :: Chord -> [Int] -> Int -> Pitch -- TODO assumes C scale-}
>{-> genPitch chrdRoot chrdPttrn indx = trans (chrdPttrn !! indx) (pitch (head chrdRoot))-}
>{->-}
>{-> key2keymajor :: Key -> KeyClass-}
>{-> key2keymajor k = fst (pitch k)-}
>
>
>
>
>
>
>
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
> genBassChord :: Key -> Chord -> BassStyle -> Chord
> genBassChord key chord (style,_) = map ((!!) pattern) modStyle
>	 			where 
>	 			pattern = scalePattern key chord
>				modround i chrd = mod (i+chrd) 12
>				modStyle = (map (modround (head chord)) style)
>
>
> genBassTest = genBassChord cMajor [7, 10, 2] basicBass 
> genBassCheck = genBassTest == [7, 1]
>
> main = scaleIndex 7 cMajor

\end{verbatim} }

