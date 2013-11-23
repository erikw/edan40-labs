module Twinkle where
import Haskore
import Data.Ratio
import Control.Exception hiding (assert)
import AutoComp

oct = (5-1) -- TODO why -1?
twinklePart1  = [((C,oct), 1), ((F,oct), 1%2), ((C,oct), 1%2), ((G,oct), 1%2), ((C,oct), 1%2), ((G,oct), 1%2), ((C,oct), 1%2)]
twinklePart2  = [((C,oct), 1%2), ((G,oct), 1%2),((C,oct), 1%2), ((G,oct), 1%2), ((C,oct), 1%2), ((G,oct), 1%2),((C,oct), 1%2), ((G,oct), 1%2)]
twinkleComp = twinklePart1 ++ twinklePart2 ++ twinklePart1
twinkleProgression = [([absPitch (pi, octc)], major, dur) | ((pi,octc),dur) <- twinkleComp]

twinkleBass = autoBass basicBass cMajor twinkleProgression 
twinkleVoicing = autoChord cMajor twinkleProgression

twinkle = Instr "piano" (Tempo 2.2 (Phrase [Dyn SF]  twinkleVoicing)) 
