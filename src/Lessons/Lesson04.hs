module Lessons.Lesson04 where

import Data.Char (isDigit)

myFold :: (b -> a -> b) -> b -> [a] -> b
myFold _ acc [] = acc
myFold f acc (h : t) = myFold f (f acc h) t

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr deriving (Show)

myE :: Expr
myE = Add (Lit 5) (Mul (Lit 3) (Lit 2))

eval :: Expr -> Int
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

instance Eq Expr where
  (Lit a) == (Lit b) = a == b
  (Add e1 e2) == (Add e3 e4) = (e1 == e3 && e2 == e4)
  (Mul e1 e2) == (Mul e3 e4) = (e1 == e3 && e2 == e4)
  _ == _ = False

class RefEq a where
  (~==~) :: a -> a -> Bool

instance RefEq Expr where
  a ~==~ b = eval a == eval b

instance RefEq Integer where
  a ~==~ b = a == b

myDiv :: Integer -> Integer -> Either String Integer
myDiv _ 0 = Left "Division by zero"
myDiv a b = Right (a `div` b)

type Error = String

parseDigit :: String -> Either Error (Char, String)
parseDigit [] = Left "Empty input"
parseDigit (h : t) = if isDigit h then Right (h, t) else Left "A digit expected"

parseTwoDigits :: String -> Either Error (Char, Char, String)
parseTwoDigits a1 =
  case parseDigit a1 of
    Left e1 -> Left e1
    Right (c1, a2) -> case parseDigit a2 of
      Left e2 -> Left ("Second element error: " ++ e2)
      Right (c2, a3) -> Right (c1, c2, a3)
