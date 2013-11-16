module Pattern where
import Utilities
import Data.Maybe(isJust, fromJust, fromMaybe)


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
match w [] [] = Just []
match w [] _ = Nothing
match w _ [] = Nothing
match w pl@(p:ps) sl@(s:ss)
    | p /= w && p /= s = Nothing
    | p /= w && p == s = match w ps ss
    | p == w =  orElse (singleWildcardMatch pl sl) (longerWildcardMatch pl sl)


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) 
                        | isJust (match wc ps xs) = Just [x]
                        | otherwise  = Nothing

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)



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
--                              wildcard -> func -> inputString -> (p1,p2)
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f input (p1, p2) = mmap ((substitute w p2) . f) (match w p1 input)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
{-transformationsApply w f plist input =  head (dropWhile (not . isJust) (map (transformationApply w f input) plist))-}
transformationsApply w f plist input =  head (((filter isJust) (map (transformationApply w f input) plist)) ++ [Nothing])


-- Test cases --------------------
transApplyExpected = "Je m'appelle Zacharias"
transApplyTestList = [("I can't *", "Jag kan inte *"), ("My name is *", "Je m'appelle *"), ("Why don't you *", "Varför gör du inte *")]
transApplyTest = transformationsApply '*' id transApplyTestList "My name is Zacharias"
transApplyCheck =  transApplyTest == Just transApplyExpected
