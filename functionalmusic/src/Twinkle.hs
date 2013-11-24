module Twinkle where
import Haskore
import Data.Ratio
import Control.Exception hiding (assert)
import AutoComp

oct = 5
twinklePart1  = [((C,oct), 1), ((F,oct), hn), ((C,oct), hn), ((G,oct), hn), ((C,oct), hn), ((G,oct), hn), ((C,oct), hn)]
twinklePart2  = [((C,oct), hn), ((G,oct), hn),((C,oct), hn), ((G,oct), hn), ((C,oct), hn), ((G,oct), hn),((C,oct), hn), ((G,oct), hn)]
twinkleComp = twinklePart1 ++ twinklePart2 ++ twinklePart1
twinkleProgression = [([absPitch (pi, octc)], major, dur) | ((pi,octc),dur) <- twinkleComp]

twinkleBass = autoBass basicBass cMajor twinkleProgression 
twinkleVoicing = autoChord cMajor twinkleProgression

twinkle = Instr "piano" (Tempo 2.2 (Phrase [Dyn SF]  twinkleBass :=: twinkleVoicing)) 
