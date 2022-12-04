import Data.Text (splitOn, unpack, pack)
import Debug.Trace

main = do
    content <- readFile ("./input.txt")
    let contentLines = lines content
    print $ part1 contentLines
    print $ part2 contentLines

data Assignment = Assignment Int Int deriving (Show)
data Pair = Pair Assignment Assignment deriving (Show)

-- Count number of pairs of assignments of elves where one fully contains the other
part1 :: [String] -> Int
part1 lines = countTrue $ map (pairFullyContains . createPair) lines
part2 :: [String] -> Int
part2 lines = countTrue $ map (pairOverlaps . createPair) lines

countTrue :: [Bool] -> Int
countTrue items = length (filter (==True) items)

pairOverlaps :: Pair -> Bool
pairOverlaps (Pair (Assignment lower1 upper1) (Assignment lower2 upper2)) = (lower1 <= lower2 && upper1 >= lower2) || (lower2 <= lower1 && upper2 >= lower1)

pairFullyContains :: Pair -> Bool
pairFullyContains (Pair first second) = (isFirstInSecond first second) || (isFirstInSecond second first)

isFirstInSecond :: Assignment -> Assignment -> Bool
isFirstInSecond (Assignment lower1 upper1) (Assignment lower2 upper2) = (lower1 >= lower2) && (upper1 <= upper2)

-- use splitOn here, but requires packing and unpacking strings into Text (compressed format)
createPair :: String -> Pair
createPair line = Pair (Assignment (read first :: Int) (read second :: Int)) (Assignment (read third :: Int) (read fourth :: Int))
    where
        [firstHalf, secondHalf] = splitOn (pack ",") (pack line)
        [first, second] = map unpack $ splitOn (pack "-") firstHalf
        [third, fourth] = map unpack $ splitOn (pack "-") secondHalf