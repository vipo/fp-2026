module Lessons.Lesson05 where

import Data.Char

type Error = String

type Parser a = String -> Either Error (a, String)

parseDigit :: Parser Char
parseDigit [] = Left "Empty input"
parseDigit (h : t) = if isDigit h then Right (h, t) else Left "A digit expected"

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 = \a1 ->
  case p1 a1 of
    Left e1 -> Left e1
    Right (v1, a2) ->
      case p2 a2 of
        Left e2 -> Left ("Second element of end2: " ++ e2)
        Right (v2, a3) -> Right ((v1, v2), a3)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 = \a1 ->
  case and2 p1 p2 a1 of
    Left e1 -> Left e1
    Right ((v1, v2), a2) -> case p3 a2 of
      Left e2 -> Left e2
      Right (v3, a3) -> Right ((v1, v2, v3), a3)

many :: Parser a -> Parser [a]
many p = many' p []
  where
    many' p' acc = \a1 ->
      case p' a1 of
        Left _ -> Right (acc, a1)
        Right (v1, a2) -> many' p' (acc ++ [v1]) a2

many1 :: Parser a -> Parser [a]
many1 p = \a1 ->
  case many p a1 of
    Left e -> Left e
    Right ([], _) -> Left "At least one element expected"
    Right a -> Right a

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = \a1 ->
  case p a1 of
    Left e -> Left e
    Right (v, a2) -> Right (f v, a2)

parseNumber :: Parser Int
parseNumber = pmap read (many1 parseDigit)

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Empty input, " ++ [c] ++ " expected")
parseChar c (h : t) = if c == h then Right (c, t) else Left ("Char " ++ [c] ++ " expected")

parseWs :: Parser Char
parseWs [] = Left "Empty input"
parseWs (h : t) = if isSpace h then Right (h, t) else Left "Whitespace expected"

parseWss :: Parser String
parseWss = many parseWs

parseWss1 :: Parser String
parseWss1 = many1 parseWs

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr deriving (Show)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = \a1 ->
  case p1 a1 of
    Right r1 -> Right r1
    Left e1 -> case p2 a1 of
      Left e2 -> Left ("Both: " ++ e1 ++ " and " ++ e2)
      Right r2 -> Right r2

parseExpr :: Parser Expr
parseExpr =
  pmap Lit parseNumber
    `orElse` pmap
      (\((_, n1), (_, n2), _) -> Add n1 n2)
      (and3 (and2 (parseOpIntro '+') parseExpr) (and2 parseWss1 parseExpr) (parseChar ')'))
  where
    parseOpIntro :: Char -> Parser ()
    parseOpIntro c = pmap (\(_, _) -> ()) (and2 (parseChar '(') (parseChar c))
