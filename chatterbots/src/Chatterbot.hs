module Chatterbot where
import Utilities
import Pattern
import System.Random
import Data.Char
import Debug.Trace

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
    rnd <- randomIO :: IO Float
    return (rulesApply ((map . map2) (id, (pick rnd)) brain))

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply ppairs phrase =  try (transformationsApply "*" reflect ppairs) phrase
{-rulesApply ppairs phrase =  trace ("ppairs: " ++ show ppairs) $ try (transformationsApply "*" reflect ppairs) phrase-}

reflect :: Phrase -> Phrase
{-reflect = id-}
{-reflect p =  map (try . (tryReplace reflections))  p-}
reflect p =  map (tryReplace reflections) p
            where
            tryReplace [] p = p
            tryReplace (r:rs) p 
                    | fst r == p = snd r
                    | otherwise = tryReplace rs p


reflectTestList =  ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
reflectTestExpected = ["you", "will", "never", "see", "your", "reflection", "in", "my", "eyes"]
reflectTest = reflect reflectTestList
reflectCheck = reflectTest == reflectTestExpected


reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile ppairs = (map . map2) ((words . map toLower), (map (words . map toLower))) ppairs


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply ppairs phrase =  fix (try (transformationsApply "*" id ppairs )) phrase
