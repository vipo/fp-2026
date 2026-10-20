module Lessons.Lesson06 where

import qualified Data.Monoid as M
import Data.Semigroup

data MySum a = MySum {getMySum :: a}

instance (Num a) => Semigroup (MySum a) where
  (MySum a) <> (MySum b) = MySum (a + b)

instance (Num a) => Monoid (MySum a) where
  mempty = MySum 0

s :: Int
s = getMySum (foldMap MySum [1, 2, 3, 4, 5])

p :: Int
p = getProduct (foldMap Product [1, 2, 3, 4, 5])

data Expr a = Lit a | Add (Expr a) (Expr a) | Mul (Expr a) (Expr a) deriving (Show)

instance Foldable Expr where
  foldMap f (Lit a) = f a
  foldMap f (Add a b) = foldMap f a <> foldMap f b
  foldMap f (Mul a b) = foldMap f a <> foldMap f b

myE :: Expr Int
myE = Add (Lit 5) (Mul (Lit 3) (Lit 2))

s' :: Int
s' = getSum (foldMap Sum myE)

p' :: Int
p' = getProduct (foldMap Product myE)

f :: Maybe Int
f = M.getFirst (foldMap (M.First . Just) myE)

even' :: Int -> Bool
even' a = a `mod` 2 == 0

toS :: Bool -> String
toS True = "taip"
toS False = "ne"

myF :: Int -> String
myF a = toS (even' a)

myF' :: Int -> String
myF' a = (toS . even') a

myF'' :: Int -> String
myF'' = toS . even'

myF''' :: Int -> String
myF''' a = toS $ even' a

type Parser e a = String -> Either e (a, String)

orElse :: (Semigroup e) => Parser e a -> Parser e a -> Parser e a
orElse p1 p2 = \a1 ->
  case p1 a1 of
    Right r1 -> Right r1
    Left e1 -> case p2 a1 of
      Left e2 -> Left $ e1 <> e2
      Right r2 -> Right r2
