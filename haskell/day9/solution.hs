import Debug.Trace
import Data.List (nub)

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let contentLines = lines content
    let moves = parseMoves contentLines

    let hPos = Pos 0 0
    let tPos = Pos 0 0
    
    let (finalHead, finalTail, finalHistory) = applyMoves moves (hPos, tPos) [tPos]
    print $ length (nub finalHistory)

data Pos = Pos Int Int deriving (Show, Read, Eq)
data Dir = U | D | L | R deriving (Show, Read, Eq)
data Move = Move Dir Int deriving (Show, Read, Eq)

applyMoves :: [Move] -> (Pos, Pos) -> [Pos] -> (Pos, Pos, [Pos])
applyMoves (move:moves) (hPos, tPos) history = applyMoves moves (hPosNew, tPosNew) uniqueHistory
    where
        (hPosNew, tPosNew, historyNew) = applyMove move (hPos, tPos) history
        uniqueHistory = nub $ history ++ historyNew
applyMoves [] (hPos, tPos) history = (hPos, tPos, history)

applyMove :: Move -> (Pos, Pos) -> [Pos] -> (Pos, Pos, [Pos])
applyMove (Move _ 0) (hPos, tPos) history = (hPos, tPos, history)
applyMove (Move dir count) (hPos, tPos) history = applyMove (Move dir (count-1)) (hPosNew, tPosNew) (history ++ [tPosNew])
    where 
        (hPosNew, tPosNew) = moveHeadThenTail dir (hPos, tPos)

moveHeadThenTail :: Dir -> (Pos, Pos) -> (Pos, Pos)
moveHeadThenTail dir (hPos, tPos) = (hPosNew, tPosNew)
    where
        hPosNew = moveHead dir hPos
        tPosNew = moveTail hPosNew tPos
        newPositions = (hPosNew, tPosNew)

moveHead :: Dir -> Pos -> Pos
moveHead U (Pos x y) = Pos x (y+1)
moveHead D (Pos x y) = Pos x (y-1)
moveHead L (Pos x y) = Pos (x-1) y
moveHead R (Pos x y) = Pos (x+1) y

-- if head and tail are only 1 square apart, then tail doesnt move, else move tail in dir of head
moveTail :: Pos -> Pos -> Pos
moveTail (Pos hPosX hPosY) tPos@(Pos tPosX tPosY) | abs (tPosX - hPosX) <= 1 && abs (hPosY - tPosY) <= 1 = tPos
moveTail (Pos hPosX hPosY) tPos@(Pos tPosX tPosY) = Pos (tPosX + xSgn) (tPosY + ySgn)
    where
        xSgn = signum (hPosX - tPosX)
        ySgn = signum (hPosY - tPosY)

parseMoves :: [String] -> [Move]
parseMoves lines = parseMovesInner lines []

parseMovesInner :: [String] -> [Move] -> [Move]
parseMovesInner (line:lines) moves = parseMovesInner lines (moves ++ [newMove])
    where
        dir = parseDirChar $ head line
        count = read (words line !! 1) :: Int
        newMove = Move dir count
parseMovesInner [] moves = moves

parseDirChar :: Char -> Dir
parseDirChar 'U' = U
parseDirChar 'D' = D
parseDirChar 'L' = L
parseDirChar 'R' = R
parseDirChar _ = error "Cannot parse dir from char"