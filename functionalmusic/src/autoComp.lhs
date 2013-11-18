\section{Auto generation of songs}
\label{autocomp}

{\small\begin{verbatim} 

> module AutoComp where
> import Haskore
> import Control.Exception hiding (assert)
> 
> data BassStyle = Basic | Calypso | Boogie  deriving (Eq, Show)
> getBassStylePatter :: BassStyle -> [Int]
> getBassStylePatter (Basic bs) = [1, 5]
>
> type ChordProgression = [[Chord]]
> ionian :: [Int] -- TODO type?
> ionian  = [0, 2, 4, 5, 7, 9, 11]
>
>
> autoBass :: BassStyle -> Key -> ChordProgression -> Music  --- TODO select scale based on key.
> autoBass (Basic bs) k cp = foldr1 :+: (map (genBassBar k (getBassStylePatter bs)) cp)
>
> genBassBar :: [Chord] -> Key -> [Int] -> Music
> genBassBar chrds key bsPatt = foldr1 :+: (map (Note (genPitch (getChrdPttrn (first chrds))) (1%2) bsPatt))
>
> getChrdPttrn :: Chord -> [Int]
> getChrdPttrn chrd = ionian
>
> genPitch :: Chord -> [Int] -> Int -> Pitch -- TODO assumes C scale
> genPitch chrdRoot chrdPttrn indx = trans (chrdPttrn !! indx) (pitch (head chrdRoot))
>
> key2keymajor :: Key -> KeyClass
> key2keymajor k = fst (pitch k)

\end{verbatim} }

