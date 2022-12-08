
import Data.Char(digitToInt)
import Debug.Trace
import System.Console.Terminfo (scrollReverse)

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let contentLines = lines content
    let grid = parseGrid contentLines []
    print $ countVisible grid
    print $ highestScore grid


data Grid = Grid { width :: Int, height :: Int, vals :: [[Int]] } deriving (Show, Read, Eq)

countVisible :: Grid -> Int
countVisible grid@(Grid width height _) = length coordsVisible
    where
        coordsVisibleOrNot = [ positionVisible grid x y | x <- [0..width-1], y <- [0..height-1]]
        coordsVisible = filter (==True) coordsVisibleOrNot

highestScore :: Grid -> Int
highestScore grid = maximum $ scenicScores grid

scenicScores :: Grid -> [Int]
scenicScores grid@(Grid width height _ ) = scores
    where
        scores = [scenicScore grid x y | x <- [0..width-1], y <- [0..height-1]]

scenicScore :: Grid -> Int -> Int -> Int
scenicScore grid x y = product unblockedHeights
    where
        height = getHeight grid x y
        
        col = getCol grid x
        above = reverse $ take y col
        below = drop (y+1) col

        row = getRow grid y
        left = reverse $ take x row
        right = drop (x+1) row

        directionsToCheck = filter (\row -> length row > 0) [above, below, left, right]

        unblockedHeights = map (unblockedChainLength height) directionsToCheck

unblockedChainLength :: Int -> [Int] -> Int
unblockedChainLength height vals = unblockedChainLengthInner height vals 0 True

unblockedChainLengthInner :: Int -> [Int] -> Int -> Bool -> Int
unblockedChainLengthInner height _ score False = score
unblockedChainLengthInner height (val:vals) score _ | val >= height = unblockedChainLengthInner height vals (score+1) False
unblockedChainLengthInner height (val:vals) score searchingFlag | val < height = unblockedChainLengthInner height vals (score+1) searchingFlag
unblockedChainLengthInner height [] score _ = score 

positionVisible :: Grid -> Int -> Int -> Bool
positionVisible grid x y = any (areHeightsSmaller height) [above, below, left, right]
    where
        height = getHeight grid x y
        
        col = getCol grid x
        above = take y col
        below = drop (y+1) col

        row = getRow grid y
        left = take x row
        right = drop (x+1) row

areHeightsSmaller :: Ord a => a -> [a] -> Bool
areHeightsSmaller height [] = True
areHeightsSmaller height heights = all (<height) heights

parseGrid :: [String] -> [[Int]] -> Grid
parseGrid (line:lines) vals = parseGrid lines (vals ++ [parseRow line []])
parseGrid [] vals = Grid (length (head vals)) (length vals) vals

parseRow :: String -> [Int] -> [Int]
parseRow (char:chars) row = parseRow chars (row ++ [digitToInt char])
parseRow [] row = row

getHeight :: Grid -> Int -> Int -> Int
getHeight grid@(Grid _ _ vals) x y = getCol grid x !! y

getRow :: Grid -> Int -> [Int]
getRow (Grid _ height _) row | row + 1 > height = error ("Cannot access row " ++ (show row) ++ " as height is " ++ (show height))
getRow (Grid _ _ vals) row = vals !! row

getCol :: Grid -> Int -> [Int]
getCol (Grid width _ _) col | col + 1 > width = error ("Cannot access col " ++ (show col) ++ " as width is " ++ (show width))
getCol (Grid _ _ vals) col = extractCol vals col []

extractCol :: [[a]] -> Int -> [a] -> [a]
extractCol (row:rows) col output = extractCol rows col (output ++ [row !! col])
extractCol [] _ output = output 