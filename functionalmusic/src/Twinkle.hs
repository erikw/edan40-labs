module Twinkle where
import Haskore
import Data.Ratio
import Control.Exception hiding (assert)
import AutoComp

oct = 5
twinklePart1  = [((C,oct), wn), ((F,oct), hn), ((C,oct), hn), ((G,oct), hn), ((C,oct), hn), ((G,oct), hn), ((C,oct), hn)]
twinklePart2  = [((C,oct), hn), ((G,oct), hn),((C,oct), hn), ((G,oct), hn), ((C,oct), hn), ((G,oct), hn),((C,oct), hn), ((G,oct), hn)]
twinkleComp = twinklePart1 ++ twinklePart2 ++ twinklePart1
twinkleProgression = [([absPitch (pi, octc)], major, dur) | ((pi,octc),dur) <- twinkleComp]

mkNote dur pi = pi dur [Volume 80]
lmap f l = line $ map f l
m1 = lmap (mkNote qn) [c oct, c oct, g oct, g oct, a oct, a oct]
            :+: mkNote hn (g oct)
            :+: lmap (mkNote qn) [f oct, f oct, e oct, e oct, d oct, d oct]
            :+: mkNote hn (c oct)
m2 = part :+: part
        where part = lmap (mkNote qn) [g oct, g oct, f oct, f oct, e oct, e oct] :+: mkNote hn (d oct)
 
twinkleMelody = m1 :+: m2 :+: m1

-- TODO experiment with temp and instruments (differnet fro bass and vocals)
twinkle = Instr "piano" (Tempo 2.2 (Phrase [Dyn SF]  twinkleMelody :=: autoComp cMajor twinkleProgression basicBass)) 
{-twinkle = Instr "piano" (Tempo 2.2 (Phrase [Dyn SF]  twinkleMelody :=: autoComp cMajor twinkleProgression calypsoBass)) -}
