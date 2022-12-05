
main :: IO ()
main = do
    content <- readFile "./input.txt"
    let contentLines = lines content

    -- Index of point in input where before this is initial setup, after this is moves to execute
    -- N.B.: set this by hand from your input!
    let splitIndex = 9

    -- The row that is the bottom of the crate diagram, with the numbering ' 1  2  3 ... ', Extract and count these
    let stackBaseRow = contentLines !! (splitIndex - 1)
    let numStacks = length $ words stackBaseRow

    let stackLines = take (splitIndex - 1) contentLines
    let movesLines = drop (splitIndex + 1) contentLines
    
    -- Track stacks as an array of arrays of Crates
    let stacks = map (parseStack stackLines) [1..numStacks]
    let moves = map parseMove movesLines

    let resultPart1 = topCrates $ applyMovesSingular moves stacks
    let resultPart2 = topCrates $ applyMovesMulti moves stacks

    print resultPart1
    print resultPart2

type Crate = Char

data Move = Move {
    num :: Int,
    start :: Int,
    end :: Int
} deriving (Show)

topCrates :: [[Char]] -> [Char]
topCrates = foldl (\acc x -> acc ++ [last x]) ""

parseMove :: String -> Move
parseMove line = Move num start end
    where
        split = words line
        num = read (split !! 1) :: Int
        start = read (split !! 3) :: Int
        end = read (split !! 5) :: Int

applyMovesMulti :: Foldable t => t Move -> [[a]] -> [[a]]
applyMovesMulti moves stacks = foldl (flip applyMoveMulti) stacks moves

applyMovesSingular :: Foldable t => t Move -> [[a]] -> [[a]]
applyMovesSingular moves stacks = foldl (flip applyMoveSingular) stacks moves

-- part 2 requires moves to happen all in one go
applyMoveMulti :: Move -> [[a]] -> [[a]]
applyMoveMulti (Move num start end) stacks = newStacks
    where
        startStack = stacks !! (start - 1)
        endStack = stacks !! (end - 1)
        newStartStack = take (length startStack - num) startStack
        newEndStack = endStack ++ drop (length startStack - num) startStack
    
        newStacksTemp = replaceStackAtIndex stacks newStartStack (start - 1)
        newStacks = replaceStackAtIndex newStacksTemp newEndStack (end - 1)

-- part 1 requires moves to happen one operation at a time
applyMoveSingular :: Move -> [[a]] -> [[a]]
applyMoveSingular (Move 0 start end) stacks = stacks
applyMoveSingular (Move num start end) stacks = applyMoveSingular (Move (num - 1) start end) newStacks
    where
        startStack = stacks !! (start - 1)
        endStack = stacks !! (end - 1)
        newStartStack = init startStack
        newEndStack = endStack ++ [last startStack]
    
        newStacksTemp = replaceStackAtIndex stacks newStartStack (start - 1)
        newStacks = replaceStackAtIndex newStacksTemp newEndStack (end - 1)

-- given a list of elems, a new elem and an index, replace the elem at index with the new elem
replaceStackAtIndex :: [a] -> a -> Int -> [a]
replaceStackAtIndex stacks newStack index = (slice 0 index stacks) ++ [newStack] ++ (slice (index + 1) (length stacks) stacks)

-- parse a vertical stack of crates at an index (dealing with empty spaces too)
parseStack :: [String] -> Int -> [Crate]
parseStack stackLines stackNum = parseStackInner stackLines stackNum []

parseStackInner :: [String] -> Int -> [Crate] -> [Crate]
parseStackInner (stackLine:xs) stackNum stack | isCrateAtLocation stackLine stackNum = parseStackInner xs stackNum (newCrate:stack)
    where
        newCrate = stackLine !! (1 + (stackNum - 1) * 4)
parseStackInner (stackLine:xs) stackNum stack | not $ isCrateAtLocation stackLine stackNum = parseStackInner xs stackNum stack
parseStackInner [] _ stack = stack

-- if we have a crate at stack here then there will be a char to represent it
isCrateAtLocation :: String -> Int -> Bool
isCrateAtLocation stackLine stackNum = startChar /= ' '
    where
        startChar = stackLine !! ((stackNum - 1) * 4)

-- reuse from day3
slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start