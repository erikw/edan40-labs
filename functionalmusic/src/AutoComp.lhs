\documentclass[10pt, a4paper]{article}
\usepackage[T1]{fontenc}
\usepackage[utf8x]{inputenc}
\usepackage{ifpdf}
\ifpdf
	\usepackage{thumbpdf}
	\usepackage[pdftex,bookmarks=true,pdftitle={TITLE},pdfauthor={Erik Westrup, ...}]{hyperref}
\else
	\usepackage{url}
\fi

\title{Functional Programming\\ EDAN40}
%\date{}		% Disable date on title page.
\author{
		\begin{tabular}{l l}
				Erik Westrup & \texttt{<ada09ewe@student.lu.se>}\\
				Jonas Klauber & \texttt{<ada09jkl@student.lu.se>}
		\end{tabular}
}

\begin{document}

\begin{titlepage}
\maketitle
\thispagestyle{empty}	% No page number on title page.
\end{titlepage}
%\setcounter{page}{2}

\section{Introduction}
In the course Functional Programming (EDAN40) the second assignment is about using data structures in Haskell. To learn how to design and use data structures we are asked to implement functions for auto generation musical elements using the Haskcore library. As with natural languages we can make some rules governing the structure of music but they does not really apply generally. Focusing on the way western music are traditionally produces using musical scores a set of rules can guide the musical structure.

\section{Auto generation of songs}
More specifically we were asked to, given a musical score for a well know song ``Twinkle, Twinkle'', transcribe it and from it generate a bass line and a vocal sounding that should be played together with the melody. To aid us in the playing and representation of musical objects we can use the free library Haskcore. Haskcore functions for representing Notes, Rest, Pitches, durations etc and functions that given a musical object outputs a MIDI file that can be played with an external MIDI player. To begin with, we import some Prelude and Haskcore modules that we will use in this module named AutoComp.


> module AutoComp where
> import Data.Ratio
> import Data.List(elemIndex)
> import Data.Maybe(fromJust)
>
> import Haskore hiding(Key)
>{-> import Control.Exception hiding (assert)-}

\section{Types \& general functions}
To make the code more readable we define some types that we will use.

The basic elements are notes. Typically these are depicted as the notes on a piano segment:

C | D | E F |G | A | B

As you have seen, this pattern is repeated on a line with the difference being the pitch of the key. Each of these segments are called an octave (here ignoring the complicated black "#-keys"). Each of these keys are called a Note and consists of a pitch and a duration. The duration is represented as a rational integer in Haskcore. And a pitch is in Haskcore represented as a pitch class and a octave where the pitch class is one of (C, D, E,...). To make the arithmetic easier there is a type called AbsPitch which is simply a integer that is the value of the pitch class plus twelve times the octave.

Every music has a key. A key determines the base tone for the song. A key has a root, which is a list of key patterns, and a harmonic quality (major or minor). In this assignment we only work with major keys so it's implicit here that they key is Major.  We also define the C Major which will be used by all our songs. More Keys could be defined for a more general program.

> type Key = [Int]
> cMajor =  [0, 2, 4, 5, 7, 9, 11] :: Key

The harmonic quality of a song determines which elements of a scale pattern to use.

> type HamonicQuality = [Int]
> major, minor :: HamonicQuality
> major = [0, 2, 4]
> minor = [5, 0, 2]

Another important type in the western musical representation is a Chord. A chord is a set of notes played simultaneously. Here we represent this as a list of pitches. A chord then can be combined with a duration.


> type Chord = [AbsPitch]


The previously mentioned scale patterns are a subset of the twelve notes in an octave. They are usually represented as a list of distances from the star note (C in C Major). There are many traditional scale patterns, which are named traditionally Ionian, Lydian, Mixodylian etc.

> scPatterns :: [[Int]]
> scPatterns = [
>		[0, 2, 4, 5, 7, 9, 11],	-- (Major)
>		[0, 2, 4, 6, 7, 9, 11],
>		[0, 2, 4, 5, 7, 9, 10],
>		[0, 2, 3, 5, 7, 8, 10], -- (Minor)
>		[0, 2, 3, 5, 7, 9, 10],
>		[0, 1, 3, 5, 7, 8, 10]
>		]


Each chord has a associated scale pattern. To find which one you look at your key and find how far from the first note in the key the first note in you chord is. Take this index in the following table. If the key is in major scale, use the first element in the tuple, otherwise the second. This value obtained is the index (starting from 1) in the previous scale pattern table that should be used. For example if you have a C Major key and a chord G-chord. You see that G is the fifth element in the C major scale (that is when you apply the scale pattern for C Major to the octave, G is the fifth choose note). Taking the first element of the fifth tuple in the scaleTable give you the number two. This means that G-chord in C Major key should be played according to the second scale pattern in the above table, named Lydian.


> -- Table mapping a key scale position to a scale pattern. (major, minor). -1 = Null.
> scaleTable :: [(Int, Int)]
> scaleTable = [(0,-1), (2,4), (-1,5), (1,-1), (2,-1), (-1, 3), (-1, -1)]


A musical score shows the traditional notes, the funny symbols, that denotes the melody to play. Further the song is divided into ``bars'', visually denoted by a bar. Each bar have one or more associated chords with it and are written just above the bar segment. This is the songs accompaniment that ``fills out'' the song and is called the chord progression. If a bar has only one chord, then that the accompaniment is generated from that one only and is played through out the whole bar. Each chord here have a harmonic quality of its own. But in our songs it happens to be that it is always Major. Finally each chord is associated with a duration.

> type ChordProgression = [(Chord, HamonicQuality,  Dur)]

To be able to debug the notes we wrote a simple function that takes a chord and prints out the notes in a human readable form.

> printChord :: Chord -> IO ()
> printChord aps = print $ map (fst . pitch) aps

To be able to perform the previous mentioned look up in scaleTable we need to be able to find the index of a give chord in a key.

> scaleIndex :: AbsPitch -> Key -> Int
> scaleIndex pitch key = fromJust $ elemIndex (mod pitch 12) key

Now that we can get the scale index for a chord in a key, we can get the actual pattern (using the helper function patternFromIndex)

> -- Get a scale pattern for a chord given a key.
> scalePattern :: Key -> Chord -> HamonicQuality -> [Int]
> scalePattern key chord harQual = patternFromIndex rootIndex harQual
> 			where rootIndex = scaleIndex (head chord) key

Here in patternFromIndex the actual work is performed. We go from a given index to the scaleTable, get a index to scalePattern where we fetch the desired scale pattern row.

> patternFromIndex :: Int -> HamonicQuality -> [Int]
> patternFromIndex index harQual = scPatterns !! scIndex
> 			where scIndex
> 				| harQual == major = fst (scaleTable !! index)
> 				| otherwise = snd (scaleTable !! index)

All indices are numbered from C as the root. But some times, e.g. when we use a chord, another root is used for the indexes. Then the numbers must wrap around so that we still can reach the notes that comes before the current note (or you can see this as a way to access the next octave).

> wrapOctave :: Int -> Int -> Int
> wrapOctave i chrd = mod (i+chrd) 12

\section{Bass}
To be able to generate the bass we need some helper functions and types. First there are different types of bass lines that fits with different songs. So we let music compose choose one of these three common styles: basic, calypso, boogie. A bass style has a list of indexes to use from the current scale pattern and a duration for each of these selected notes. In more complex bass styles one could imagine that each note having a possibly different duration but for all our three styles it's possible to give the same durations to each.

> type BassStyle = ([Int], Dur)
> basicBass, calypsoBass, boogieBass :: BassStyle
> basicBass = ([0,4], hn)
> calypsoBass = ([-1, -1, 0, 2, -1, -1, 0, 2], en)
> boogieBass = ([0, 4, 5, 4, 0, 4, 5, 4], en)

For each chord in the chord progession, 1) we want to get the associated pattern 2) determine how many of the notes from the pattern to use 3) pick these many notes from the bass style 4) use these picked notes to pick from the scale pattern.

> genBassChord :: Key ->  BassStyle -> (Chord, HamonicQuality, Dur) -> Chord
> genBassChord key (style,_) (chord, harQual, crtio) = map (pickNote modPattern) redStyle
>	 			where
>	 			pattern = scalePattern key chord harQual
>				modPattern = map (wrapOctave (head chord)) pattern
>	 			notesToPlay = (round $ (rtof crtio) * (float (length style)))
>	 			redStyle = take notesToPlay style
>				oct =  snd (pitch $ head chord)
>				pickNote pttr bassPttrIndex
>					| bassPttrIndex == -1 = bassPttrIndex
>					| otherwise = oct * 12 + (pttr !! bassPttrIndex)


Now that we can generate a chord we just have to transform its elements to music and play them in in sequence.

> genBassChordNote :: Key ->  BassStyle -> (Chord, HamonicQuality, Dur) -> Music
> genBassChordNote key style@(s, dur) bassChord =  foldr1 (:+:) [mkNote pi | pi <- chord]
> 					where
> 					chord = genBassChord key style bassChord
> 					mkNote pi
> 						| pi == -1 = Rest (dur)
> 						| otherwise = Note (pitch pi) (dur) [Volume 50]
>

Finally we can write the function that generates the whole bass line given a bass style, a key and a chord progresstion. It simply plays in sequence the results from applying genBassChordNote to each note in the chord progression.

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass _ _ [] = c 0 0 [Volume 0]
> autoBass style key cprog = foldr1 (:+:) (map (genBassChordNote key style) cprog)
>

\section{Voicing}

To make the music richer than just having the bass we add a generated voicing to the song. The chord voicing is built on triads chords i.e. chords with three elements. These triads are generated from the chord progression. There are many ways of choosing what triad chord to play. In the instructions we are given some heuristics on how to generated a good voicing. The first one is to keep the chords in the range (E,4)-(G,5). So we wrote a function that takes a pitch and moves it into the desired range. This is possible since notes in different octaves are ``equivalent'' but sounds different.

> limitPitch :: AbsPitch -> AbsPitch
> limitPitch pi
> 	| pi < (pitchClass E + 4 * 12) = limitPitch (pi + 12)
> 	| pi > (pitchClass G + 5 * 12) = limitPitch (pi - 12)
> 	| otherwise = pi


Previously in the bass generation, when we used a chord we only used the root of it i.e. the first element. Now we need the whole chord sequence. Given a key, chord root and a harmonic quality we can generate the full chord. I.e. the chord that is an argument has length 1 and the one returned has length 3. This function is similar to the previous genBassChord in that it gets a scale pattern from the chord.  The indices in the harmonic qulity are used to pick elements in the scale pattern. Some octave modulation and value preservation has to be done to be able to search for indexes and in the end keep the original octave (the map (+ chrod) preserves the original octave).

> makeChord :: Key -> Chord -> HamonicQuality -> Chord
> makeChord key chord harQual = map ((+) (head chord)) (map ((!!) modPattern) harQual)
> 			where
>			modPattern = map (wrapOctave (head chord)) scPattern
>	 		scPattern = scalePattern key chord harQual

So from a given chord (full sequence now) we want to be able to find a good chord to play. In the instructions it says that we are free to change the order of the elements in a triad. We want to find one that has the smallest distance from the previously played chord. To cope with this we simply generate all possible inversions of a given triad.

> -- A list of all chord inversions from a given chord. Assuming a chord is a triad
> chordInversions :: Chord -> [Chord]
> chordInversions orig@(c1:c2:c3') =  [orig, [c1,c3,c2], [c2,c1,c3], [c2,c3,c1], [c3,c1,c2], [c3,c2,c1]]
> 					where c3 = head c3' -- TODO ugly

The semitonal distances between two chords is just the absolute sum of the positional differences in pitches.

> semiDistance :: Chord -> Chord -> Int
> semiDistance a b = sum $ map abs (zipWith (-) a b)

Now we can write a function that given a chord find the best inversion to play. This function starts with generating all chord inversions, applies the semiDistance function to all of them and find the smallest one. Then the smallest one is picked and limited to the desired range.


> bestChord :: Chord -> Chord -> Chord
> bestChord prevChrd chord =  limInversions !! (fromJust $ elemIndex minVal invDists)
> 				where
> 				minVal = minimum invDists
> 				invDists = map (semiDistance prevChrd) inversions
> 				inversions = chordInversions chord
> 				limInversions = map (map limitPitch) inversions
>
>

This helper function takes a chord and a duration and generate music by playing the notes in parallel.

> chrdComp :: Chord -> Dur -> Music
> chrdComp chrd dur = foldr1 (:=:) [Note (pitch pi) dur [Volume 50] | pi <- chrd]

The main function for generating the voicing is the genChord function. It processes each chord in the chord progression one at a time. The chords are made to music one by one (by recursion) and it is made sure that all members of a chord are played simultaneously. Before this we need to find good voicing triads which make the song sound better. This is generated by considering the previous chord. Also we have to generate the full chord sequence(which was was not needed in the base generation).

> genChord :: Key -> Chord -> ChordProgression -> Music
> genChord _ _ [] = c 0 0 [Volume 0]
> genChord _ [] ((chord, harQual, dur):cps) =  chrdComp chord dur
> genChord key prevChrd ((chord, harQual, dur):cps) =  (chrdComp nextChord dur):+: genChord key nextChord cps
> 				where
> 				nextChord = bestChord prevChrd fullChord
> 				fullChord = makeChord key chord harQual


This function simply starts the generation of the voicing gets music in return. Since there was no previously played note, the previous note is set to the empty list.

> autoChord :: Key -> ChordProgression -> Music
> autoChord key cp = genChord key [] cp

\section{Accompaniment}

The full accompaniment can now easily be generated by playing the bass and voicing simultaneously.

> autoComp :: Key -> ChordProgression -> BassStyle -> Music
> autoComp key cprog bstyle = autoBass bstyle key cprog :=: autoChord key cprog


\end{document}
