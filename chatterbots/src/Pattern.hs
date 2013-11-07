module Pattern where
import Utilities
import Data.Maybe(isJust, fromJust)


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
--                  wildcard -> p -> s
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute w [] _ = []
substitute w (p:ps) s
                | p == w = s ++ substitute w ps s
                | otherwise = p : substitute w ps s


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
--              wildcard t s
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
{-match w [] [] = Nothing-}
{-match w [] s = Nothing-}
match w [] [] = Just []
match w [] _ = Nothing
match w p [] = Nothing
match w pl@(p:ps) sl@(s:ss)
    | p /= w && p /= s = Nothing
    | p /= w && p == s = match w ps ss
    | p == w =  orElse (singleWildcardMatch pl sl) (longerWildcardMatch pl sl)


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch [] [] = Just []
{-singleWildcardMatch (wc:ps) (x:xs)-}
            {-| wc == x = Just (wc :  singleWildcardMatch ps xs)-}
            {-| otherwise = Nothing-}
            {-= match (wc:ps) (x:xs)-}
singleWildcardMatch (wc:ps) (x:xs) = if isJust (match wc ps xs ) then Just [x] else Nothing 


{-longerWildcardMatch (wc:ps) (x:xs) =  if isJust (\res -> res = (match wc (wc:ps) xs)) then Just ([x] : (fromJust res)) else Nothing-}
longerWildcardMatch (wc:ps) (x:xs) = Just (x : fromJust (match wc (wc:ps) xs))



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}

