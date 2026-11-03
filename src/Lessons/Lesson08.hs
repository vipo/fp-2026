{-# LANGUAGE InstanceSigs #-}

module Lessons.Lesson08 where

import Control.Applicative
import Data.Char

data Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Left e -> Left e
      Right (v, r) -> Right (f v, r)

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

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \input -> Left $ "Empty parse: " ++ input
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = Parser $ \input ->
    case runParser p1 input of
      Right r1 -> Right r1
      Left e1 ->
        case runParser p2 input of
          Right r2 -> Right r2
          Left e2 -> Left $ e1 ++ " OR " ++ e2

digit :: Parser Char
digit = Parser $ \input ->
  case input of
    [] -> Left "Empty input"
    (h : t) -> if isDigit h then Right (h, t) else Left "A digit expected"

char :: Char -> Parser Char
char c = Parser $ \input ->
  case input of
    [] -> Left "Empty input"
    (h : t) -> if h == c then Right (h, t) else Left $ "Char expected " ++ [c]

chari :: Char -> Parser Char
chari c = char (toLower c) <|> char (toUpper c)

drop' :: Parser ()
drop' = (\_ _ _ _ -> ()) <$> chari 'D' <*> chari 'R' <*> chari 'O' <*> chari 'P'

drop'' :: Parser ()
drop'' = chari 'D' *> chari 'R' *> chari 'O' *> chari 'P' *> pure ()

from :: Parser ()
from = chari 'F' *> chari 'R' *> chari 'O' *> chari 'M' *> pure ()

table :: Parser ()
table = chari 'T' *> chari 'A' *> chari 'B' *> chari 'L' *> chari 'E' *> pure ()

select :: Parser ()
select = chari 'S' *> chari 'E' *> chari 'L' *> chari 'E' *> chari 'C' *> chari 'T' *> pure ()

ws :: Parser Char
ws = Parser $ \input ->
  case input of
    [] -> Left "Empty input"
    (h : t) -> if isSpace h then Right (h, t) else Left "Whitespace expected"

letter :: Parser Char
letter = Parser $ \input ->
  case input of
    [] -> Left "Empty"
    (h : t) -> if isAlpha h then Right (h, t) else Left "A letter expected"

identifier :: Parser String
identifier = (:) <$> letter <*> many (letter <|> digit)

data Column = All | Named String deriving (Show)

data Statement
  = Select [Column] String
  | Drop String
  deriving (Show)

column :: Parser Column
column = fmap (\_ -> All) (char '*') <|> fmap Named identifier

columns :: Parser [Column]
columns = (:) <$> column <*> many (many ws *> char ',' *> many ws *> column)

statement :: Parser Statement
statement =
  Drop <$> (many ws *> drop'' *> some ws *> table *> some ws *> identifier)
    <|> Select <$> (many ws *> select *> some ws *> columns) <*> (many ws *> from *> some ws *> identifier)
