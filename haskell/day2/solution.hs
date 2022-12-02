import Debug.Trace

-- Rock paper scissors
main = do
    content <- readFile ("./input.txt")
    let contentLines = lines content
    let part1 = map processLinePart1 contentLines
    print $ sum part1
    let part2 = map processLinePart2 contentLines
    print $ sum part2

processLinePart1 (opponent:_:self:_) = gameScore opponent self

processLinePart2 (opponent:_:self:_) = gameScore opponent (part2Selection self opponent)

gameScore opponent self = (roundScore opponent self) + (selectionScore self)

-- A = rock, B = paper, C = scissors
-- X = rock, Y = paper, Z = scissors
-- 0 = loss, 3 = draw, 6 = win

roundScore 'A' 'X' = 3
roundScore 'A' 'Y' = 6
roundScore 'A' 'Z' = 0

roundScore 'B' 'X' = 0
roundScore 'B' 'Y' = 3
roundScore 'B' 'Z' = 6

roundScore 'C' 'X' = 6
roundScore 'C' 'Y' = 0
roundScore 'C' 'Z' = 3

selectionScore 'X' = 1
selectionScore 'Y' = 2
selectionScore 'Z' = 3

part2Selection 'X' opponent | trace ("Loosepick for " ++ [opponent] ++ " is " ++ [loosePick opponent]) True = loosePick opponent
part2Selection 'Y' opponent | trace ("Drawpick for " ++ [opponent] ++ " is " ++ [drawPick opponent]) True = drawPick opponent 
part2Selection 'Z' opponent | trace ("Winpick for " ++ [opponent] ++ " is " ++ [winPick opponent]) True = winPick opponent

-- what to pick if you want to loose
loosePick 'A' = 'Z'
loosePick 'B' = 'X'
loosePick 'C' = 'Y'

-- what to pick if you want to win
winPick 'A' = 'Y'
winPick 'B' = 'Z'
winPick 'C' = 'X'

-- what to pick if you want to draw
drawPick 'A' = 'X'
drawPick 'B' = 'Y'
drawPick 'C' = 'Z'



