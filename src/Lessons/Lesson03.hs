module Lessons.Lesson03 where

f :: Int -> Int
f a = 45345 * (455 * a)

f' :: Int -> Int
f' a =
  let k = 45 * a
      l = k * k
      m = 9
   in l - m

f'' :: Int -> Int
f'' a = l - 2 * m
  where
    k = 45 * a
    l = k * k
    m = 9

null :: [a] -> Bool
null a = case a of
  [] -> True
  (_ : _) -> False

mv :: [a] -> Bool -> Int
mv a b = case (a, b) of
  ([], True) -> 42
  (_, False) -> 45
  _ -> 0

mv' :: Int -> Int -> Bool
mv' a b = case [a, b] of
  [42, _] -> True
  _ -> False

mv'' :: Bool -> Int
mv'' b = case b of
  True -> 42
  _ -> 0

goodLen :: [a] -> Int
goodLen a = goodLen' a 0
  where
    goodLen' :: [a] -> Int -> Int
    goodLen' [] acc = acc
    goodLen' (_ : t) acc = goodLen' t (1 + acc)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f''' (h : t) = (f''' h) : myMap f''' t

addOne :: Int -> Int
addOne a = a + 1

mySum :: [Int] -> Int
mySum a = foldl (\acc el -> acc + el) 0 a

myConcat :: [String] -> String
myConcat a = foldl (\acc el -> acc ++ el) "" a

lenSum :: [String] -> Int
lenSum a = foldl (\acc el -> acc + length el) 0 a

evens :: [Int] -> [Int]
evens a = filter (\el -> el `mod` 2 == 0) a

add :: Int -> Int -> Int
add a b = a + b

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr deriving (Show)

myE :: Expr
myE = Add (Lit 5) (Mul (Lit 3) (Lit 2))

eval :: Expr -> Int
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
