import Data.List

main :: IO ()
main = do
  content <- readFile ("./input.txt")
  let scores = calcCalories (lines content) 0 []
  print scores
  print $ maximum scores
  print $ sum $ map (nthLargest scores) [0, 1, 2]

-- recurse along list from left to right, restarting total when we reach a ""
calcCalories :: [String] -> Integer -> [Integer] -> [Integer]
calcCalories ("":xs) score scores = calcCalories xs 0 (score : scores)
calcCalories (x:xs) score scores = calcCalories xs (score + (read x ::Integer)) scores
calcCalories [] score scores = score : scores

nthLargest xs n = (reverse $ sort xs) !! n