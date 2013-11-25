module Twinkle where
import Haskore
import Data.Ratio
import Control.Exception hiding (assert)
import AutoComp

oct = 5
jesusComp  = [((C,oct), wn), ((C,oct), wn),((F,oct), wn),((C,oct), wn),((C,oct), wn),((C,oct), wn),((F,oct), hn),((C,oct), hn),((G,oct), hn),((C,oct), hn),((C,oct), wn),((F,oct), wn),((C,oct), wn),((G,oct), wn),((C,oct), wn),((F,oct), wn),((C,oct), hn),((G,oct), hn),((C,oct), wn)]
jesusProgression = [([absPitch (pi, octc)], major, dur) | ((pi,octc),dur) <- jesusComp]

mkNote dur pi = pi dur [Volume 80]
lmap f l = line $ map f l
m1 = lmap (mkNote qn) [g oct, e oct, e oct, d oct, e oct, g oct]
            :+: mkNote hn (g oct)
m2 = lmap (mkNote qn) [a oct, a oct, c (oct+1), a oct, a oct, g oct]
            :+: mkNote hn (g oct)
            :+: lmap (mkNote qn) [g oct, e oct, e oct, d oct] 
m3 = lmap (mkNote qn) [e oct, g oct]
            :+: mkNote hn (g oct)
            :+: lmap (mkNote qn) [a oct, a oct, g oct, c oct, e oct, d oct]
            :+: mkNote hn (c oct)
m4 = mkNote hn (g oct)
            :+: lmap (mkNote qn) [e oct, g oct, a oct]
            :+: mkNote dhn (c (oct+1))
            :+: mkNote hn (g oct)
            :+: lmap (mkNote qn) [e oct, c oct]
m5 = mkNote qn (e oct)
            :+: mkNote dhn (d oct)
            :+: mkNote hn (g oct)
            :+: lmap (mkNote qn) [e oct, g oct, a oct]
            :+: mkNote hn (c (oct+1))
            :+: mkNote qn (a oct)
m6 = lmap (mkNote qn) [g oct, c oct, e oct, d oct]

jesusMelody = m1 :+: m2 :+: m3 :+: m4 :+: m5 :+: m6

jesus = Instr "piano" (Tempo 2.2 (Phrase [Dyn SF]  jesusMelody :=: autoComp cMajor jesusProgression basicBass)) 
