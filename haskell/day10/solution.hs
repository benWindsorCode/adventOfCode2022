import Data.List (isPrefixOf)
import Debug.Trace

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let contentLines = lines content
    let instructions = parseInstructions contentLines
    let reg = Register 'X' 1 Noop
    let regHistory = executeInstructions instructions reg

    -- print contentLines
    print instructions
    print "\n\n"
    print $ zip [1..500] regHistory
    print $ sum $ map (signalStrength regHistory) [20, 60, 100, 140, 180, 220]

data Instruction = Addx { cyclesLeft :: Int, addValue :: Int } | Noop deriving (Show)
data Register = Register { label :: Char, regValue :: Int, lastAction :: Instruction } deriving (Show)

signalStrength :: [Register] -> Int -> Int
signalStrength registerHistory cycle = regValue reg * cycle
    where
        reg = registerHistory !! (cycle - 1)

parseInstructions :: [String] -> [Instruction]
parseInstructions lines = parseInstructionsInner lines []

parseInstructionsInner :: [String] -> [Instruction] -> [Instruction]
parseInstructionsInner (line:lines) instructions = parseInstructionsInner lines (instructions ++ [parseInstruction line])
parseInstructionsInner [] instructions = instructions

parseInstruction :: [Char] -> Instruction
parseInstruction line 
    | "noop" `isPrefixOf` line = Noop
    | "addx" `isPrefixOf` line = parseAddx line

parseAddx :: String -> Instruction
parseAddx line = Addx 2 addValue
    where
        [_, addValueStr] = words line
        addValue = read addValueStr :: Int

executeInstructions :: [Instruction] -> Register -> [Register]
executeInstructions instructions register = executeInstructionsInner instructions register []

executeInstructionsInner :: [Instruction] -> Register -> [Register] -> [Register]
executeInstructionsInner (instruction:instructions) register history | trace ("OUTER executing: " ++ show instruction) True = executeInstructionsInner instructions currentRegisterValue (history ++ newRegisterHistory)
    where
        (newRegisterHistory, currentRegisterValue) = executeInstruction instruction register
executeInstructionsInner [] _ history = history

-- return list of values register goes through while instruction executed AND the value of the register after cycle comlete (because Addx doesnt actually affect until the start of next cycle)
executeInstruction :: Instruction -> Register -> ([Register], Register)
executeInstruction Noop (Register label regValue _) | trace ("Executing: " ++ show Noop) True = ([Register label regValue Noop], Register label regValue Noop)
executeInstruction add@(Addx _ _) reg | trace ("Executing: " ++ show add) True = executeAddx add reg []

executeAddx :: Instruction -> Register -> [Register] -> ([Register], Register)
executeAddx instruction@(Addx cyclesLeft addValue) reg@(Register label regValue _) regHistory
    | cyclesLeft > 1 = executeAddx (Addx (cyclesLeft - 1) addValue) (Register label regValue instruction) (regHistory ++ [Register label regValue instruction])
    | cyclesLeft == 1 = (regHistory ++ [Register label regValue instruction], Register label (regValue + addValue) instruction)