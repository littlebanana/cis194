{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case w of
    ("I":t:m)   -> LogMessage Info (read t) (unwords m)
    ("W":t:m)   -> LogMessage Warning (read t) (unwords m)
    ("E":i:t:m) -> LogMessage (Error $ read i) (read t) (unwords m)
    _           -> Unknown (unwords w)
  where w = words s

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) Leaf = Leaf
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node lt r@(LogMessage _ tt _) rt) = if t < tt then Node (insert m lt) r rt else Node lt r (insert m rt)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m $ build ms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt m rt) = inOrder lt ++ [m] ++ inOrder rt

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error i) _ _) = if i >= 50 then True else False
isSevereError (LogMessage _ _ _) = False
isSevereError (Unknown _) = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ m) = m
extractMessage (Unknown m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong m = map extractMessage (filter isSevereError (inOrder (build m)))
