module Chatterbot where
import Utilities
import System.Random
import Data.Char

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

-- map2 applied on (single) botBrain: tuple (id Phrase, pick random Phrase from list)
-- map: above on whole list of botBrain
-- rulesApply: on all resulting phrases
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind botBrain = do
  r <- randomIO :: IO Float
  return $ rulesApply $ (map . map2) (id, pick r) botBrain

-- try, since we want some result
rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = try . transformationsApply "*" reflect

-- flip swaps the two first arguments that follow
-- lookup gets a 'value' for a corresponding 'key' in a list of tuples
reflect :: Phrase -> Phrase
reflect = map $ try $ flip lookup reflections

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

-- Make all chars lowercase, except for 'special characters'.
-- Apply reduction on results after converting string to list
prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

-- Create the BotBrain by compiling a structure such as the one given in Eliza.hs
rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = (map . map2) (words . map toLower, map words)


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

-- transformationsApply with id just returns the first phrase that is
-- successfully reduced in the list of PhrasePairs.
-- fix is used to 'reapply' this process on the list until it no longer
-- changes when it is applied.
reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . transformationsApply "*" id 


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wc (x:xs) ys
  | x /= wc       = x: substitute wc xs ys
  | otherwise     = ys ++ substitute wc xs ys


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc (p:ps) (x:xs)
  | p == x        = match wc ps xs
  | p /= wc       = Nothing
  | otherwise     = singleWildcardMatch (p:ps) (x:xs)
                    `orElse` longerWildcardMatch (p:ps) (x:xs)

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]

-- At least one x == wc. We need to return a list. If match returns Nothing,
-- then we need to return Nothing. Shouldn't have to define edge cases since
-- those are taken care of in match.
singleWildcardMatch (wc:ps) (x:xs)  = mmap (const [x]) $ match wc ps xs
longerWildcardMatch (wc:ps) (x:xs)  = mmap (x:) $ match wc (wc:ps) xs


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
transformationApply wc f xs (p1, p2) = mmap (substitute wc p2) $ mmap f $ match wc p1 xs


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply _ _ _ [] = Just []
transformationsApply wc f patternList xs = foldl1 orElse $ map (transformationApply wc f xs) patternList


