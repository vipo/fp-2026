module Lessons.Lesson01 where

i :: Int
i = 42

ii :: Integer
ii = 5634875683746583645873648563485763487563487563478

s :: String
s = "labas"

b' :: Bool
b' = True

f :: Float
f = 3.8

c :: Char
c = 'd'

d :: Double
d = 44.333

l :: [Int]
l = []

k :: [Int]
k = 23 : l

j :: [Int]
j = 42 : k

m :: [Int]
m = 52 : l

m' :: [Int]
m' = 54 : 53 : 52 : []

m'' :: [Int]
m'' = [54, 53, 52]

add :: Int -> Int -> Int
add a b = a + b

t :: (Int, String)
t = (42, "")

t' :: (Integer, Bool, Char)
t' = (1, False, 'a')

snd' :: (a, b, c) -> b
snd' (_, v2, _) = v2
