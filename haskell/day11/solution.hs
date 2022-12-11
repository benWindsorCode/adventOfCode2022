import Data.List (sort, isPrefixOf)
import Debug.Trace

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let contentLines = lines content
    let groups = groupLines contentLines [] []
    let monkeys = map parseGroup groups
    let firstMonkey = head monkeys
    print $ processRound monkeys 
    print $ processRounds 20 monkeys
    print $ part1MonkeyBusiness (processRounds 20 monkeys)

type Item = Int
data Test = DivBy Int deriving (Show)
data Operation = Square | MultBy Int | AddTo Int deriving (Show)
data Monkey = Monkey { id :: Int, items :: [Item], operation :: Operation, test :: Test, trueTarget :: Int, falseTarget :: Int, inspections :: Int } deriving (Show)

part1MonkeyBusiness :: [Monkey] -> Int
part1MonkeyBusiness monkeys = first * second
    where
        inspectionCounts = map inspections monkeys
        [first, second] = take 2 $ reverse $ sort inspectionCounts

processRounds :: (Eq t, Num t) => t -> [Monkey] -> [Monkey]
processRounds 0 monkeys = monkeys
processRounds numRounds monkeys = processRounds (numRounds-1) (processRound monkeys)

processRound :: [Monkey] -> [Monkey]
processRound = processRoundInner 0

-- given current index and monkeys process round until index reaches end of list
processRoundInner :: Int -> [Monkey] -> [Monkey]
processRoundInner monkeyId updatedMonkeys | monkeyId < length updatedMonkeys = processRoundInner (monkeyId + 1) (processThrowsForMonkey updatedMonkeys monkey (items monkey))
    where
        monkey = updatedMonkeys !! monkeyId
processRoundInner _ updatedMonkeys = updatedMonkeys

processThrowsForMonkey :: [Monkey] -> Monkey -> [Item] -> [Monkey]
processThrowsForMonkey monkeys monkey@(Monkey currentId _ _ _ _ _ _) (item:itemsLeft) = processThrowsForMonkey newMonkeys monkey itemsLeft
    where
        (nextMonkeyId, newItem) = updatedMonkeyAndItem monkey item
        newMonkeysTemp = moveItem monkeys item newItem currentId nextMonkeyId 
        updatedCurrentMonkey = incrementInspections (newMonkeysTemp !! currentId)
        newMonkeys = replaceAtIndex newMonkeysTemp updatedCurrentMonkey currentId
        
processThrowsForMonkey monkeys _ [] = monkeys

updatedMonkeyAndItem :: Monkey -> Item -> (Int, Item)
updatedMonkeyAndItem monkey@(Monkey _ items operation test trueTarget falseTarget _) item 
    | item `elem` items = (nextMonkeyId, worryScoreUpdated)
    | item `notElem` items = error ("Item: " ++ show item ++ " not owned by monkey: " ++ show monkey)
    where
        worryScore = applyOperation operation item
        worryScoreUpdated = newWorryScore worryScore
        nextMonkeyId = applyTest worryScoreUpdated test trueTarget falseTarget

applyOperation :: Operation -> Int -> Int
applyOperation Square item = item * item
applyOperation (MultBy val) item = val * item
applyOperation (AddTo val) item = val + item

newWorryScore :: Integral a => a -> a
newWorryScore score = quot score 3

applyTest :: Int -> Test -> Int -> Int -> Int
applyTest score (DivBy factor) trueTarget falseTarget | divisibleBy factor score = trueTarget
applyTest score (DivBy factor) trueTarget falseTarget | not $ divisibleBy factor score = falseTarget

moveItem :: [Monkey] -> Item -> Item -> Int -> Int -> [Monkey]
moveItem monkeys oldItem newItem sourceIdx targetIdx = newMonkeys
    where
        sourceMonkey = monkeys !! sourceIdx
        targetMonkey = monkeys !! targetIdx

        updatedSourceMonkey = removeItemFromMonkey sourceMonkey oldItem
        updatedTargetMonkey = addItemToMonkey targetMonkey newItem

        tempMonkeys = replaceAtIndex monkeys updatedSourceMonkey sourceIdx
        newMonkeys = replaceAtIndex tempMonkeys updatedTargetMonkey targetIdx

incrementInspections :: Monkey -> Monkey 
incrementInspections (Monkey id items operation test trueTarget falseTarget inspections) = Monkey id items operation test trueTarget falseTarget (inspections+1)

addItemToMonkey :: Monkey -> Item -> Monkey
addItemToMonkey (Monkey id items operation test trueTarget falseTarget inspections) item = Monkey id newItems operation test trueTarget falseTarget inspections
    where
        newItems = items ++ [item]
        
removeItemFromMonkey :: Monkey -> Item -> Monkey
removeItemFromMonkey (Monkey id items operation test trueTarget falseTarget inspections) item = Monkey id newItems operation test trueTarget falseTarget inspections
    where
        newItems = filter (/=item) items

groupLines :: [String] -> [String] -> [[String]] -> [[String]]
groupLines ("":xs) currentGroup groups = groupLines xs [] (groups ++ [currentGroup])
groupLines (x:xs) currentGroup groups = groupLines xs (currentGroup ++ [x]) groups
groupLines [] currentGroup groups = groups ++ [currentGroup]

-- given a group of 6 lines in form:
-- money number, starting items, operation, test, If true, If false
-- parse into a MonkeyAction object
parseGroup :: [String] -> Monkey
parseGroup (first:second:third:fourth:fifth:sixth:xs) | trace ("parsing: " ++ first) (null xs) = Monkey id items operation test trueTarget falseTarget inspections
    where
        id = parseId first
        items = parseItems second
        operation = parseOperation third
        test = parseTest fourth 
        trueTarget = parseTarget fifth
        falseTarget = parseTarget sixth 
        inspections = 0

parseId :: String -> Int
parseId line | trace ("Parsing id: " ++ line) True = read (split !! 1) :: Int
    where
        -- remove the trailing ':'
        stripped = init line
        split = words stripped

parseItems :: String -> [Item]
parseItems line | trace ("Parsing items: " ++ line) True = read listStr :: [Int]
    where
        end = stripPrefix line "  Starting items: "
        listStr = "[" ++ end ++ "]"

parseOperation :: String -> Operation
parseOperation line | trace ("Parsing operation: " ++ line) True = createOperation op val
    where
        end = stripPrefix line "  Operation: new = old "
        -- after stripping prefix, left with just the e.g. '+ 19' or '* 38' bit 
        [op, val] = words end

parseTest :: String -> Test
parseTest line| trace ("Parsing test: " ++ line) True = DivBy (read end :: Int)
    where
        end = stripPrefix line "  Test: divisible by "

parseTarget :: String -> Int
parseTarget line 
    | "    If true" `isPrefixOf` line = read (stripPrefix line "    If true: throw to monkey ") :: Int 
    | "    If false" `isPrefixOf` line = read (stripPrefix line "    If false: throw to monkey ") :: Int 

createOperation :: String -> String -> Operation
createOperation "*" "old" = Square
createOperation "*" val = MultBy (read val :: Int)
createOperation "+" val = AddTo (read val :: Int)
createOperation other _ = error ("Cannot create operation from operator: " ++ other)

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy factor input = (input `mod` factor) == 0

multBy :: Num a => a -> a -> a
multBy mult input = mult * input

addTo :: Num a => a -> a -> a
addTo add input = add + input

-- There is a version of this using Maybe monad https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List.html#v:stripPrefix
-- but not sure if here it just adds complexity... so for now fail fast if cannot strip prefix
stripPrefix :: String -> String-> String
stripPrefix line prefix | prefix `isPrefixOf` line = drop (length prefix) line
stripPrefix line prefix = error ("CANNOT remove prefix: " ++ prefix ++ " is not prefix of " ++ line)

-- reuse from day3
slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

-- reused from day 5
replaceAtIndex :: [a] -> a -> Int -> [a]
replaceAtIndex stacks newStack index = (slice 0 index stacks) ++ [newStack] ++ (slice (index + 1) (length stacks) stacks)
