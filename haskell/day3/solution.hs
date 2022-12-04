import Debug.Trace
import Data.Char
import Data.List

main = do
    content <- readFile ("./input.txt")
    let contentLines = lines content
    print $ part1 contentLines
    print $ part2 contentLines

part1 :: [String] -> Int
part1 lines = sum (map scoreRucksack lines)

part2 :: [String] -> Int
part2 lines = part2Inner (map uniqueChars lines) 0

part2Inner :: [String] -> Int -> Int
part2Inner (first:second:third:xs) score = part2Inner xs (score + getPriority common)
    where
        common = commonItem first second third
part2Inner [] score = score

-- given three rucksacks find the common item
commonItem :: [Char] -> String -> String -> Char
commonItem (x:xs) second third 
    | isCharInBothStrings x second third = x
    | not $ isCharInBothStrings x second third = commonItem xs second third
commonItem [] second thirs = error "rucksacks have no item in common"

-- iterate over rucksack, each char in first half gets a score if its also in second half
scoreRucksack :: String -> Int
scoreRucksack rucksack = scoreRucksackInner (uniqueChars $ firstHalf rucksack) (uniqueChars $ secondHalf rucksack) 0

scoreRucksackInner :: [Char] -> String -> Int -> Int
scoreRucksackInner (x:xs) half score = scoreRucksackInner xs half (score + (scoreChar x half))
scoreRucksackInner [] _ score = score

-- a char gets a score of it is in the rucksack 
scoreChar :: Char -> String -> Int
scoreChar char string 
    | isCharInString char string = getPriority char
    | not $ isCharInString char string = 0

-- return just a string of the unique chars in a string
uniqueChars :: [Char] -> String
uniqueChars string = uniqueCharsInner string ""

uniqueCharsInner :: [Char] -> String -> String
uniqueCharsInner (x:xs) chars 
    | isCharInString x chars = uniqueCharsInner xs chars
    | not $ isCharInString x chars = uniqueCharsInner xs (x:chars)
uniqueCharsInner [] chars = chars

isCharInBothStrings :: Char -> String -> String -> Bool
isCharInBothStrings char string1 string2 = (isCharInString char string1) && (isCharInString char string2)

isCharInString :: Char -> (String -> Bool)
isCharInString char = any (==char)

-- convert a-z and A-Z to their respective priorities
getPriority :: Char -> Int
getPriority char 
    | isUpper char = upperScores !! (fromJust $ findIndex (==char) upper)
    | not $ isUpper char = lowerScores !! (fromJust $ findIndex (==char) lower)
    where
        lower = ['a'..'z']
        upper = ['A'..'Z']
        lowerScores = [1..26]
        upperScores = [27..52]

firstHalf :: String -> String
firstHalf input = slice start mid input
    where
        total = length input
        mid = total `div` 2
        start = 0

secondHalf :: [a] -> [a]
secondHalf input = slice mid total input
    where
        total = length input
        mid = total `div` 2
        start = 0

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

-- Borrowed from here as couldnt work out the import https://hackage.haskell.org/package/strict-0.3.2/docs/Data-Strict-Maybe.html#v%3AfromJust
fromJust :: Maybe a -> a
fromJust Nothing  = error "Data.Strict.Maybe.fromJust: Nothing"
fromJust (Just x) = x