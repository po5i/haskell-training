{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- exercise 1
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"
-- TEST: testParse parse 10 "error.log"
parseMessage :: String -> LogMessage
parseMessage m =  case splitted of
                    ("I":ts:rest) -> LogMessage Info (read ts :: Int) (unwords rest)
                    ("W":ts:rest) -> LogMessage Warning (read ts :: Int) (unwords rest)
                    ("E":lv:ts:rest) -> LogMessage (Error (read lv :: Int)) (read ts :: Int) (unwords rest)
                    _ -> Unknown m
                  where splitted = words m

parse :: String -> [LogMessage]
-- didn't work:
-- parse file = parseMessage (head linesOfFile) : parse $ unlines $ tail linesOfFile
--              where linesOfFile = lines file
-- using map (duh!)
parse file = map parseMessage (lines file)

-- exercise 2
-- which inserts a new LogMessage into an existing MessageTree
-- producing a new MessageTree.
insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg Leaf = Node Leaf logMsg Leaf  -- first element
insert logMsg@(LogMessage _ ts _) (Node left nodeLogMsg@(LogMessage _ tsi _) right)
  | ts > tsi = Node left nodeLogMsg (insert logMsg right)
  | otherwise = Node (insert logMsg left) nodeLogMsg right
insert _ tree = tree  -- Unknowns

-- exercise 3
-- builds up a MessageTree containing the messages in the list,
-- by successively inserting the messages into a MessageTree
-- (beginning with a Leaf)
build :: [LogMessage] -> MessageTree
-- build list = foldr insert Leaf list
build = foldr insert Leaf


-- exercise 4
-- takes a sorted MessageTree and produces a list of all the
-- LogMessages it contains, sorted by timestamp from smallest to biggest.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

-- exercise 5
-- takes an unsorted list of LogMessages
-- returns a list of the messages corresponding to any errors with a severity of
-- 50 or greater
-- sorted by timestamp
isError50 :: LogMessage -> Bool
isError50 (LogMessage (Error lv) _ _) = lv >= 50
isError50 _ = False

filterList :: [LogMessage] -> [LogMessage]
filterList = filter isError50

getErrorMsg :: LogMessage -> String
getErrorMsg (LogMessage _ _ msg) = msg
getErrorMsg _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getErrorMsg . filterList . inOrder . build
