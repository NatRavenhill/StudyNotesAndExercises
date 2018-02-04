{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

test :: String
test = "I 2194 little creature, and held out its arms and legs in all directions, 'just"

testE :: String
testE = "E 31 225 [drmfs:4 ink isable wilaticatus re(TMEM"

testList :: [LogMessage]
testList = [LogMessage (Error 20000) 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled",LogMessage (Error 345) 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,",LogMessage Info 858 "your pocket?' he went on, turning to Alice.",LogMessage Info 898 "would be offended again.",LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)",LogMessage (Error 34)790 "those long words, and, what's more, I don't believe you do either!' And",LogMessage Info 3899 "hastily."]
  
--Exercise 1--

getMessage :: String -> MessageType
getMessage ('I' : _) = Info
getMessage ('W' : _) = Warning 
getMessage ('E' : tl) = Error (read (head (words tl)) :: Int)
getMessage _ = error "not a message"

getTimestamp :: String -> TimeStamp
getTimestamp s
  | head (words s) == "I" || head (words s) == "W" = read (head (tail (words s))) :: Int
  | head (words s) == "E" = read (head (tail (tail (words s)))) :: Int
  | otherwise =  error "not a timestamp"

getRest :: String -> String
getRest s
    | head (words s) == "I" || head (words s) == "W"  = unwords (tail ( tail (words s)))
    | head (words s) == "E" = unwords (tail (tail (tail (words s))))
    | otherwise  = error "not a string"


parseMessage :: String -> LogMessage
parseMessage s
  | (head s == 'I') || (head s == 'W') || (head s == 'E')  = LogMessage (getMessage s) (getTimestamp s) (getRest s)
  | otherwise = Unknown s

--Exercise 2--
--parses an entire file of log messages and returns its contents as a list of log messages

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)  


--Exercise 2--

sampleTree :: MessageTree
sampleTree = Node Leaf (LogMessage Info 5000000 "somestuff") Leaf

--inserts a new LogMessage into an existing MessageTree assumign a sorted MessageTree

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(Unknown _) t = t 
insert m Leaf  = Node Leaf m Leaf
insert _ t@(Node _ (Unknown _ ) _) = t 
insert msg@(LogMessage _ m _ ) (Node l msg2@(LogMessage _ n _) r)
  | (m <= n) = Node (insert msg l) msg2 r
  | otherwise = Node l msg2 (insert msg r)

-- Exercise 3--
--builds a message Tree from a list of LogmMessages

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m : ms) = help (m:ms) Leaf
    where
      help :: [LogMessage] -> MessageTree -> MessageTree
      help [] t = t
      help (n:ns) t  = help ns (insert n t)

--Exercise 4--
--takes a sorted MessageTree and produces a list of its log messages
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

--Exercise 5--
--List of error messages when error higher than 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error n )  _ s) : ms)
 | (n > 50) = s : whatWentWrong ms
 | otherwise = whatWentWrong ms
whatWentWrong (m : ms) = whatWentWrong ms
