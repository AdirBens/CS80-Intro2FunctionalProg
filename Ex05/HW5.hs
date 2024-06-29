{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror Deque.hs HW5.hs
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Control.Applicative (liftA2)
import Data.Char (chr, ord, toLower, toUpper)
import Data.Either
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import Data.Set qualified as S
import Deque (Deque)
import Deque qualified as DQ

data FoldMapFunc a m result = FoldMapFunc {agg :: a -> m, finalize :: m -> result}

foldMap' :: (Foldable t, Monoid m) => FoldMapFunc a m result -> t a -> result
foldMap' FoldMapFunc{agg, finalize} = finalize . foldMap agg

-- Section 1: Foldable functions
fmsum :: Num a => FoldMapFunc a (Sum a) a
fmsum = FoldMapFunc Sum getSum

fmor :: FoldMapFunc Bool Any Bool
fmor = FoldMapFunc Any getAny

fmfold :: Monoid a => FoldMapFunc a a a
fmfold = FoldMapFunc id id

fmelem :: Eq a => a -> FoldMapFunc a Any Bool
fmelem x = FoldMapFunc (Any . (== x)) getAny

fmfind :: (a -> Bool) -> FoldMapFunc a (First a) (Maybe a)
fmfind f = FoldMapFunc (\x -> if f x then First (Just x) else First Nothing) getFirst

fmlength :: FoldMapFunc a (Sum Int) Int
fmlength = FoldMapFunc (const (Sum 1)) getSum

fmnull :: FoldMapFunc a All Bool
fmnull = FoldMapFunc (const $ All False) getAll

fmmaximum :: (Ord a) => FoldMapFunc a (Maybe(Max a)) (Maybe a)
fmmaximum = FoldMapFunc (Just . Max) (fmap getMax)

fmminimum :: (Ord a) => FoldMapFunc a (Maybe(Min a)) (Maybe a)
fmminimum = FoldMapFunc (Just . Min) (fmap getMin)

fmmaxBy :: Ord b => (a -> b) -> FoldMapFunc a (Maybe (Max (b, a))) (Maybe a)
fmmaxBy f = FoldMapFunc (Just . Max . (\x -> (f x, x))) (fmap (snd . getMax))

fmminBy :: Ord b => (a -> b) -> FoldMapFunc a (Maybe (Min (b, a))) (Maybe a)
fmminBy f = FoldMapFunc (Just . Min . (\x -> (f x, x))) (fmap (snd . getMin))

fmtoList :: FoldMapFunc a [a] [a]
fmtoList = FoldMapFunc (: []) id

-- Section 2: Deque instances (Don't forget to implement the instances in Deque.hs as well!)
newtype DequeWrapper a = DequeWrapper (Deque a) deriving (Show, Eq)

instance Semigroup (DequeWrapper a) where
  (DequeWrapper dq1) <> (DequeWrapper dq2) = DequeWrapper (dq1 <> dq2)

instance Monoid (DequeWrapper a) where
  mempty = DequeWrapper DQ.empty

instance Foldable DequeWrapper where
  foldMap f (DequeWrapper dq) = foldMapD dq
    where
      foldMapD dq' = case DQ.popl dq' of
        Nothing -> mempty
        Just (x, dq'') -> f x <> foldMapD dq''

instance Functor DequeWrapper where
  fmap f (DequeWrapper dq) = DequeWrapper (fmapD dq)
    where
      fmapD dq' = case DQ.popr dq' of
        Nothing -> DQ.empty
        Just (x, dq'') -> DQ.pushr (f x) (fmapD dq'')

instance Applicative DequeWrapper where
  pure x = DequeWrapper (DQ.pushl x DQ.empty)
  liftA2 f (DequeWrapper d1) (DequeWrapper d2) = DequeWrapper (loopD1 d1 d2)
    where
      loopD1 dq1 dq2 = case DQ.popl dq1 of
        Just (x, rest) -> combine (loopD2 x dq2) (loopD1 rest dq2)
        Nothing -> DQ.empty

      loopD2 x1 dq = case DQ.popl dq of
        Just (x2, rest) -> DQ.pushl (f x1 x2) (loopD2 x1 rest)
        Nothing -> DQ.empty

      combine dq1 dq2 = case DQ.popl dq1 of
        Just (x, rest) -> DQ.pushl x (combine rest dq2)
        Nothing -> dq2

instance Monad DequeWrapper where
  return = pure
  (>>=) = flip foldMap

-- Section 3: Calculator and traverse
class Monad f => CalculatorError f where
  divideByZero :: f Int
  missingVariable :: String -> f Int

runCalculator :: CalculatorError f => Map String Int -> Expr -> f Int
runCalculator vars = go
  where
    go = \case
      Val x -> pure x
      Var x -> maybe (missingVariable x) pure (vars !? x)
      Add x y -> liftA2 (+) (go x) (go y)
      Sub x y -> liftA2 (-) (go x) (go y)
      Mul x y -> liftA2 (*) (go x) (go y)
      Div x y -> do
        y' <- go y
        if y' == 0 then divideByZero else liftA2 div (go x) (pure y')

-- Instances to implement:
instance CalculatorError Maybe where
  missingVariable _ = Nothing
  divideByZero = Nothing

data Err = DivByZero | MissingVar String deriving (Show, Eq)

instance CalculatorError (Either Err) where
  missingVariable = Left . MissingVar
  divideByZero = Left DivByZero


data Defaults
  = Defaults
  -- This replaces the entire division result, not just the denominator!
  { defaultForDivisionByZero :: Int
  , defaultForVariable :: String -> Int
  }

instance CalculatorError (Reader Defaults) where
  divideByZero = Reader $ \Defaults {defaultForDivisionByZero} -> defaultForDivisionByZero
  missingVariable missingVar = Reader $ \Defaults {defaultForVariable} -> defaultForVariable missingVar
-- From the lectures:
newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f r = Reader $ f . runReader r

instance Applicative (Reader r) where
  pure = Reader . const
  liftA2 f ra rb = Reader $ \r -> f (runReader ra r) (runReader rb r)

instance Monad (Reader r) where
  ra >>= f = Reader $ \r -> runReader (f $ runReader ra r) r

data Expr
  = Val Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- Section 4: Hangman
type Score = Int

getGuess :: IO Char
getGuess = do
  line <- getLine
  case line of
    [] -> getGuess
    (c:_) -> return c

isAlphabet :: Char -> Bool
isAlphabet c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

remainingLetters :: Set Char -> String
remainingLetters guessedLetters = "[" ++ S.toList (S.difference (S.fromList ['a' .. 'z']) guessedLetters) ++ "]"

updateSecret :: String -> Set Char -> String
updateSecret word guessedLetters = map revealLetters word
  where
    revealLetters c
      | c == ' ' = ' '
      | toLower c `elem` guessedLetters = c
      | otherwise = '_'
      
hangman :: String -> IO Score
hangman word = do
  let nUniqueLetters = S.size . S.fromList $ filter isAlphabet $ map toLower word
  let guessedLetters = S.fromList []
  let tries = 0
  let nHits = 0

  hangmanLoop word nUniqueLetters guessedLetters tries nHits "Guess a letter : "

hangmanLoop :: String -> Int -> Set Char -> Int -> Int -> String -> IO Score
hangmanLoop word nUniqueLetters guessedLetters nTries nHits status = do  
  if nHits == nUniqueLetters
    then do
      putStrLn "Very good, the word is: "
      putStrLn word
      return (nTries - nUniqueLetters)

  else do
    putStrLn $ updateSecret word guessedLetters
    putStrLn status
    guess <- getGuess

    -- valid alpha input
    if isAlphabet guess
      then do
        let guess' = toLower guess
        let updatedGuessedLetters = S.fromList $ S.toList guessedLetters ++ [guess']

        -- case of hit!
        if guess' `elem` map toLower word
          then do
            -- case already guessed
            if guess' `elem` guessedLetters
              then do
                hangmanLoop word nUniqueLetters updatedGuessedLetters nTries nHits "Guess a letter: "

            else do
                hangmanLoop word nUniqueLetters updatedGuessedLetters (1 + nTries) (1 + nHits) "Guess a letter: "

        -- case of miss!
        else do
          putStrLn "Wrong guess!"
          hangmanLoop word nUniqueLetters updatedGuessedLetters (1 + nTries) nHits "Try again: "

    else if guess == '?'
      then do
        putStr "Remaining letters: "
        putStrLn (remainingLetters guessedLetters)
        hangmanLoop word nUniqueLetters guessedLetters nTries nHits "Guess a letter: "

    else do
      putStrLn ("Invalid letter guess " ++ [guess] ++ "!")
      hangmanLoop word nUniqueLetters guessedLetters nTries nHits "Try again: "
