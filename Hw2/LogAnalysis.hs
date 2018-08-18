{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Text.Read (readMaybe)
import Log

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage msg = parseMsg (words msg)

parseMsg :: [String] -> LogMessage
parseMsg l@("E":code:stamp:msg) = case readMaybe code of
  Just n  -> tryMsg (Error n) stamp msg
  Nothing -> Unknown (unwords l)
parseMsg ("W":stamp:msg) = tryMsg Warning stamp msg
parseMsg ("I":stamp:msg) = tryMsg Info stamp msg
parseMsg l = Unknown (unwords l)

tryMsg :: MessageType -> String -> [String] -> LogMessage
tryMsg msgType stamp msg = case readMaybe stamp of
  Just time -> LogMessage msgType time (unwords msg)
  Nothing   -> Unknown (unwords (stamp:msg))

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert LogMessage{} tree@(Node _ Unknown{} _) = tree
insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ti _) (Node l this@(LogMessage _ tt _) r) =
  if ti <= tt
    then Node (insert msg l) this r
    else Node l this (insert msg r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . inOrder . build . filter (filterError 50)

filterError :: Int -> LogMessage -> Bool
filterError lvl (LogMessage mType _ _) = case mType of
  Error n   -> n >= lvl
  _ -> False
filterError _ Unknown{} = False

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg
getMsg (Unknown msg) = msg
