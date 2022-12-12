import Data.List (elemIndex)
main :: IO ()
main = do
    content <- readFile "./test.txt"
    let contentLines = lines content
    print contentLines
    print $ parseRow (head contentLines)
    print $ parseGrid contentLines

data Grid = Grid { width :: Int, height :: Int, vals :: [[Int]], start :: (Int, Int), goal :: (Int, Int) } deriving (Show, Read, Eq)

parseGrid :: [String] -> Grid
parseGrid lines = Grid width height vals start goal
    where
        vals = foldl (\grid row -> grid ++ [parseRow row]) [] lines
        width = length (head lines)
        height = length lines
        start = findFirstCoords lines 'S'
        goal = findFirstCoords lines 'E'

-- TODO
findFirstCoords lines target = (0,0)

parseRow :: String -> [Int]
parseRow = foldl (\row char -> row ++ [parseChar char]) []

parseChar :: Char -> Int
parseChar char 
    | char == 'S' = parseChar 'a'
    | char == 'E' = parseChar 'z'
parseChar char = fromJust $ elemIndex char ['a'..'z']

-- Borrowed from here as couldnt work out the import https://hackage.haskell.org/package/strict-0.3.2/docs/Data-Strict-Maybe.html#v%3AfromJust
fromJust :: Maybe a -> a
fromJust Nothing  = error "Data.Strict.Maybe.fromJust: Nothing"
fromJust (Just x) = x