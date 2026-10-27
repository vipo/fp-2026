{-# LANGUAGE InstanceSigs #-}

module Lessons.Lesson07 where

import Data.Char

data Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

parseDigit :: Parser Char
parseDigit = Parser $ \input ->
  case input of
    [] -> Left "Empty input"
    (h : t) -> if isDigit h then Right (h, t) else Left "A digit expected"

add2 :: Parser a -> Parser b -> Parser (a, b)
add2 p1 p2 = Parser $ \a1 ->
  case runParser p1 a1 of
    Left e1 -> Left e1
    Right (v1, a2) ->
      case runParser p2 a2 of
        Left e2 -> Left e2
        Right (v2, a3) -> Right ((v1, v2), a3)

e1 :: Either String Int
e1 = fmap (+ 1) $ Right 45

e2 :: Either Int Int
e2 = fmap (+ 1) $ Right 45

e3 :: Either Int Int
e3 = fmap (+ 1) $ Left 45

data MyEither a b = MyLeft a | MyRight b

instance Functor (MyEither a) where
  fmap _ (MyLeft r) = MyLeft r
  fmap f (MyRight r) = MyRight (f r)

m1 :: Maybe Int
m1 = fmap (+ 1) $ Just 4

m2 :: Maybe Int
m2 = fmap (+ 1) Nothing

l1 :: [Int]
l1 = fmap (+ 1) [1, 2, 3, 4]

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Left e -> Left e
      Right (v, r) -> Right (f v, r)

parseSingleNum :: Parser Int
parseSingleNum = fmap (\c -> read [c]) parseDigit

l2 :: [Int]
l2 = pure 4

m3 :: Maybe Int
m3 = pure 4

e4 :: Either String Int
e4 = pure 4

m4 :: Maybe Int
m4 = (+) <$> Just 4 <*> Just 6

m5 :: Maybe Int
m5 = (\a b c -> a + b * c) <$> Just 4 <*> Just 6 <*> Just 3

m6 :: Maybe Int
m6 = (+) <$> Nothing <*> Just 6

m7 :: Maybe (Int -> Int)
m7 = (\a b c -> a + b * c) <$> Just 4 <*> Just 6

m8 :: Maybe (Int -> Int -> Int)
m8 = (\a b c -> a + b * c) <$> Just 4

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  af <*> aa = Parser $ \input ->
    case runParser af input of
      Left e1 -> Left e1
      Right (f, r1) ->
        case runParser aa r1 of
          Left e2 -> Left e2
          Right (a, r2) -> Right (f a, r2)

and2' :: Parser a -> Parser b -> Parser (a, b)
and2' p1 p2 = (,) <$> p1 <*> p2

and2'' :: Maybe Int -> Maybe String -> Maybe (Int, String)
and2'' m1 m2 = (,) <$> m1 <*> m2

and2''' :: [Int] -> [String] -> [(Int, String)]
and2''' m1 m2 = (,) <$> m1 <*> m2
