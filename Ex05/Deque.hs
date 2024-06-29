{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Deque (Deque, empty, pushr, pushl, popr, popl) where

import Control.Applicative (liftA2)
import Data.Foldable

empty :: Deque a
empty = Deque [] []

data Deque a = Deque [a] [a]

instance Eq a => Eq (Deque a) where
  Deque l r == Deque l' r' = l ++ reverse r == l' ++ reverse r'
instance Show a => Show (Deque a) where
  show (Deque l r) = show (l ++ reverse r)

pushl :: a -> Deque a -> Deque a
pushl x (Deque l r) = Deque (x : l) r

pushr :: a -> Deque a -> Deque a
pushr x (Deque l r) = Deque l (x : r)

popl :: Deque a -> Maybe (a, Deque a)
popl = \case
  Deque [] [] -> Nothing
  Deque (l : ls) r -> Just (l, Deque ls r)
  Deque [] r -> popl $ Deque (reverse r) []

popr :: Deque a -> Maybe (a, Deque a)
popr = \case
  Deque [] [] -> Nothing
  Deque l (r : rs) -> Just (r, Deque l rs)
  Deque l [] -> popr $ Deque [] (reverse l)

instance Semigroup (Deque a) where
  (Deque l1 r1) <> (Deque l2 r2) = Deque (l1 ++ reverse r2) (r1 ++ l2)

instance Monoid (Deque a) where
  mempty = empty

instance Foldable Deque where
  foldr f z (Deque l r) = foldr f (foldr f z r) l

instance Functor Deque where
  fmap f (Deque l r) = Deque (fmap f l) (fmap f r)
  
instance Applicative Deque where
  pure a = Deque [a] []
  (Deque fs gs) <*> (Deque xs ys) = Deque (zipWith ($) fs xs) (zipWith ($) gs ys)
  
instance Monad Deque where
  return = pure
  (>>=) = flip foldMap