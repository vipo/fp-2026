module Lessons.Lesson02 where

data FireExtinguisherType = A | B | C deriving (Show, Eq)

isItC :: FireExtinguisherType -> Bool
isItC A = False
isItC B = False
isItC C = True

data FireExtinguisher = MkFireExtinguisher Int FireExtinguisherType deriving Show

isItBig :: FireExtinguisher -> Bool
isItBig (MkFireExtinguisher c _) = c >= 10

data FireExtinguisher2 = FireExtinguisher2 {
 capacity :: Int,
 feType :: FireExtinguisherType 
} deriving Show

f1 :: FireExtinguisher2
f1 = FireExtinguisher2 10 A

isItA :: FireExtinguisher2 -> Bool
isItA fe = if feType fe == A then True else False

data Some = C1 Int | C2 | C3 String

l :: [a] -> Int
l [] = 0
l (_:t) = 1 + l t

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h:_) = Just h

type IntToString = [(Int, String)]

mapping :: IntToString
mapping = [(1, "vienas"), (2, "du")]

find :: IntToString -> Int -> Maybe String
find [] _ = Nothing
find ((k1, v):t) k2 = if k1 == k2 then Just v else find t k2

orDefault :: Maybe a -> a -> a
orDefault Nothing d = d
orDefault (Just v) _ = v
