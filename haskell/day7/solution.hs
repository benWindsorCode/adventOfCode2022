import Data.List (isPrefixOf)


-- UNFINISHED ------
main :: IO ()
main = do
    content <- readFile "./test.txt"
    let contentLines = lines content
    let commands = parseCommands contentLines
    print contentLines
    print commands

data Command = Cd { dir :: String } | Ls [Entry] deriving (Show, Read, Eq)
data Entry = Dir String | File Int String deriving (Show, Read, Eq)
data Tree = EmptyTree | Node Entry [Tree] deriving (Show, Read, Eq)

createTree :: [Command] -> Tree
createTree commands = createTreeInner commands EmptyTree []

createTreeInner (command:commands) EmptyTree history | isCd command && not (isPrevDir command) = createTreeInner commands (Node (Dir (dir command)) [EmptyTree]) history
createTreeInner (command:commands) (Node entry tree) history | isCd command && isPrevDir command = createTreeInner commands (last history) (init history)
createTreeInner (command:commands) (Node entry tree) history | isLs command = createTreeInner commands 
createTreeInner [] _ history = head history

createNodeFromLs (Ls entries) = createNodeFromLsInner entries 

createNodeFromLsInner (entry:entries) 

parseCommands :: [String] -> [Command]
parseCommands lines = parseCommandsInner lines []

parseCommandsInner :: [String] -> [Command] -> [Command]
parseCommandsInner (line:lines) commands | "$ cd" `isPrefixOf` line = parseCommandsInner lines (commands ++ [newCommand])
    where
        newCommand = Cd (extractDir line)
parseCommandsInner (line:lines) commands | "$ ls" `isPrefixOf` line = parseCommandsInner linesLeft (commands ++ [newCommand])
    where
        (newCommand, linesLeft) = parseLsCommand lines []

parseCommandsInner [] commands = commands

parseLsCommand :: [String] -> [Entry] -> (Command, [String])
parseLsCommand lines@(line:rest) entries | "$" `isPrefixOf` line = ( Ls entries, lines )
parseLsCommand (line:lines) entries = parseLsCommand lines (newEntry:entries)
    where
        newEntry = parseEntry line
parseLsCommand []  entries = (Ls entries, [])

-- recieve an entry line (so it doesnt start with $) and parse into file name or dir
parseEntry :: String -> Entry
parseEntry line | "dir" `isPrefixOf` line = Dir dirName
    where
        dirName = words line !! 1
parseEntry line | not $ isPrefixOf "dir" line = File size name
    where
        [sizeStr, name] = words line
        size = read sizeStr :: Int
parseEntry line | "$" `isPrefixOf` line = error ("Cannot parse entry: " ++ line)

extractDir :: String -> String
extractDir line = words line !! 2

isPrevDir :: Command -> Bool
isPrevDir (Cd val) = val == ".."
isPrevDir _ = False

isCd :: Command -> Bool
isCd (Cd _) = True
isCd _ = False

isLs :: Command -> Bool
isLs (Ls _) = True
isLs _ = False

isDir :: Entry -> Bool
isDir (Dir _) = True
isDir _ = False