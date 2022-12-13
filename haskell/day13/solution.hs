import Debug.Trace

-- UNFINISHED
main :: IO ()
main = do
    content <- readFile "./test.txt"
    let contentLines = lines content
    let groups = groupLines contentLines [] []
    print contentLines
    print groups
    let testList = "[1,2,4,5]"
    print $ parseNestedList (tail testList) (List [])

data NestedList = Elem Int | List [NestedList] deriving (Show)

-- TODO this is not finished
parseNestedList :: String -> NestedList -> NestedList
parseNestedList ('[':rest) (List elems) | trace "Parsing: [" True = List (elems ++ [parseNestedList rest (List [])])

parseNestedList (x:',':rest) (List elems) | trace ("Parsing: " ++ [x]) (isDigit x) = parseNestedList rest (List (elems ++ [Elem (read [x] :: Int)]))
parseNestedList (x:']':rest) (List elems) | trace ("Parsing: " ++ [x]) (isDigit x) = parseNestedList rest (List (elems ++ [Elem (read [x] :: Int)]))

parseNestedList (']':rest) list | trace "Parsing: ]" True = list
parseNestedList [] list | trace "List complete" True = list
parseNestedList other list = error ("Couldnt process: " ++ other ++ ", progress: " ++ show list)

isDigit :: Char -> Bool
isDigit char = char `elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']

-- reuse line grouping from day11
groupLines :: [String] -> [String] -> [[String]] -> [[String]]
groupLines ("":xs) currentGroup groups = groupLines xs [] (groups ++ [currentGroup])
groupLines (x:xs) currentGroup groups = groupLines xs (currentGroup ++ [x]) groups
groupLines [] currentGroup groups = groups ++ [currentGroup]    