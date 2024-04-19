-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
-- import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||))
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||), last)

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- ********* --
-- Section 1
-- ********* --
const :: a -> b -> a
const x _ = x

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) f g = g . f

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f x y z = f y z x

lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f x y z = f z x y

-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:) f g x y = f . g x y

(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:.) f g x = f .: g x

(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::) f g x = f .:. g x

(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.::.) f g x = f .:: g x

-- How can we ever implement such a function!?
impossible :: a -> b
impossible _ = undefined

-- ********* --
-- Section 2
-- ********* --
countDigits :: Integer -> Integer
countDigits n
    | n > 9 = countDigits (n `div` 10) + 1
    | n < -9 = countDigits ((-n) `div` 10) + 1
    | otherwise = 1

toBinary :: Integer -> Integer
toBinary 0 = 0
toBinary 1 = 1
toBinary n
    | n < 0 = -toBinary (-n)
    | otherwise = n `mod` 2 + (toBinary (n `div` 2) * 10)


fromBinary :: Integer -> Integer
fromBinary 0 = 0
fromBinary 1 = 1
fromBinary n
    | n < 0 = -fromBinary (-n)
    | otherwise = (n `mod` 10) + fromBinary (n `div` 10) * 2


isAbundant :: Integer -> Bool
isAbundant n | n <= 0 = False
isAbundant n = divisorsSum 1 > n
  where
    divisorsSum i | i > n `div` 2 = 0
    divisorsSum i = if n `mod` i == 0 then i + divisorsSum (i + 1) else divisorsSum (i + 1)

exponent :: Integer -> Integer
exponent n
    | n == 0 = 1
    | otherwise = 10 * exponent (n - 1)

rotateDigits :: Integer -> Integer
rotateDigits n
     | n >= 0    = (n `div` exponent (countDigits n - 1)) + ((n `mod` exponent (countDigits n - 1)) * 10)
     | otherwise = -(((-n) `div` 10) + (((-n) `mod` 10) * exponent (countDigits (-n) - 1)))

-- ********* --
-- Section 3
-- ********* --
type Generator a = (a -> a, a -> Bool, a)

nullGen :: Generator a -> Bool
nullGen (_, predicate, seed) = not $ predicate seed

lastGen :: Generator a -> a
lastGen (logic, predicate, seed) =
  if nullGen (logic, predicate, seed)
    then seed
    else lastGen (logic, predicate, logic seed)

lengthGen :: Generator a -> Int
lengthGen (logic, predicate, seed) =
  let nextGen = (logic, predicate, logic seed)
   in if nullGen (logic, predicate, seed) then 0 else 1 + lengthGen nextGen

sumGen :: Generator Integer -> Integer
sumGen (logic, predicate, seed) =
  if nullGen (logic, predicate, seed)
    then 0
    else
      let nextElement = logic seed
          nextGen = (logic, predicate, nextElement)
       in if nullGen nextGen then nextElement else nextElement + sumGen nextGen


type Predicate a = a -> Bool

anyGen :: Predicate a -> Generator a -> Bool
anyGen predicate (logic, condition, seed) =
  let nextElement = logic seed
      nextGen = (logic, condition, nextElement)
   in (not (nullGen (logic, condition, seed)) && (predicate nextElement || anyGen predicate nextGen))


allGen :: Predicate a -> Generator a -> Bool
allGen predicate (logic, condition, seed) =
  let nextElement = logic seed 
      nextGen = (logic, condition, nextElement)
   in if nullGen nextGen then predicate nextElement else predicate nextElement && allGen predicate nextGen


noneGen :: Predicate a -> Generator a -> Bool
noneGen predicate generator = not $ anyGen predicate generator

countGen :: Predicate a -> Generator a -> Int
countGen predicate (logic, condition, seed) =
  let nextElement = logic seed
      nextGen = (logic, condition, nextElement)
      countPredicate = if predicate nextElement then 1 else 0
   in if nullGen nextGen then countPredicate else countPredicate + countGen predicate nextGen

-- ********* --
-- Section 4
-- ********* --

isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | n == 2  || n == 3 = True
    | even n = False
    | otherwise = allGen (\x -> n `mod` x /= 0) ((+ 2), (< (n `div` 2) + 1), 1)

isSemiprime :: Integer -> Bool
isSemiprime n = countGen (\x -> n `mod` x == 0 && isPrime x && isPrime (n `div` x)) ((+ 1), (<= n `div` 2), 2) > 0


goldbachPair :: Integer -> (Integer, Integer)
goldbachPair n = 
  let findPair i = if isPrime i && isPrime (n - i) then (i, n - i) else findPair (i + 1)
   in findPair 2

goldbachPair' :: Integer -> (Integer, Integer)
goldbachPair' n = 
  let findPair i = if isPrime i && isPrime (n - i) then (n - i, i) else findPair (i - 1)
   in findPair (n `div` 2)

-- ***** --
-- Bonus
-- ***** --
isCircularPrime :: Integer -> Bool
isCircularPrime n = allGen isPrime (rotateDigits', (/= n), n)

--Create new rotateDigits that doesnt get rid of the zero digit
rotateDigits' :: Integer -> Integer
rotateDigits' n
     | n >= 0 = (n `div` 10) + ((n `mod` 10) * exponent (countDigits n - 1))
      | otherwise = -(((-n) `div` 10) + (((-n) `mod` 10) * exponent (countDigits (-n) - 1)))