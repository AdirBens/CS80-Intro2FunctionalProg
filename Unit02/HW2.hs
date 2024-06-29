{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------
-- Functions added from the lectures
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just $ f x
maybeMap _ Nothing = Nothing

tail :: [a] -> [a]
tail [] = error "empty list"
tail (_:xs) = xs

-- Section 1.1: Basic Maybes
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap f (Just x) = f x
concatMaybeMap _ Nothing = Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe x Nothing = x

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing = x
maybe _ f (Just y) = f y

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs


-- Section 1.2 Basic Eithers
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap _ (Left err) = Left err
concatEitherMap f (Right x) = f x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right y) = Right y

catEithers :: [Either e a] -> Either e [a]
catEithers [] = Right []
catEithers (x:xs) = case x of
  Left err -> Left err
  Right y -> case catEithers xs of
    Left err -> Left err
    Right ys -> Right (y:ys)

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither _ [] = Right []
mapEither f (x:xs) = case f x of
  Left err -> Left err
  Right b -> case mapEither f xs of
    Left err -> Left err
    Right bs -> Right (b:bs)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers (x:xs) = case x of
  Left a -> case partitionEithers xs of
    (as, bs) -> (a:as, bs)
  Right b -> case partitionEithers xs of
    (as, bs) -> (as, b:bs)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b


-- -- Section 2: Lists
take :: Int -> [a] -> [a]
take _ [] = []
take n (x:xs)
    | n <= 0 = []
    | otherwise = x : take (n-1) xs
    
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
    | f x = x : takeWhile f xs
    | otherwise = []

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n xs
    | n <= 0 = xs
    | otherwise = drop (n-1) (tail xs)

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs)
    | f x = dropWhile f xs
    | otherwise = x:xs
    
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

rotate :: Int -> [a] -> [a]
rotate n xs 
    | n <= 0 = xs
    | otherwise = reverse(take newN (reverse xs)) ++ reverse(drop newN (reverse xs))
        where newN = n `mod` length xs

lotate :: Int -> [a] -> [a]
lotate n xs
    | n <= 0 = xs
    | otherwise = drop newN xs ++ take newN xs
        where newN = n `mod` length xs 

type Generator a = (a -> a, a -> Bool, a)

fromGenerator :: Generator a -> [a]
fromGenerator (next, stop, x)
    | stop x = next x : fromGenerator (next, stop, next x)
    | otherwise = []

replicate :: Int -> a -> [a]
replicate n x
    | n <= 0 = []
    | otherwise = x : replicate (n-1) x

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)


-- -- Section 3: zips and products
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill _ _ [] [] = []
zipFill defA defB [] (y:ys) = (defA, y) : zipFill defA defB [] ys
zipFill defA defB (x:xs) [] = (x, defB) : zipFill defA defB xs []
zipFill defA defB (x:xs) (y:ys) = (x, y) : zipFill defA defB xs ys

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail [] [] = Right []
zipFail [] _ = Left ErrorFirst
zipFail _ [] = Left ErrorSecond
zipFail (x:xs) (y:ys) =
  case zipFail xs ys of
    Right pairs -> Right ((x, y) : pairs)
    Left err -> Left err

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y):pairs) = case unzip pairs of
  (xs, ys) -> (x:xs, y:ys)


-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)

-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]

-- KnightMoves Translation using cases
moveKnight :: KnightPos -> KnightMove -> KnightPos
moveKnight (KnightPos x y) move = case move of
  TopLeft     -> KnightPos (x - 2) (y - 1)
  TopRight    -> KnightPos (x + 2) (y - 1)
  BottomLeft  -> KnightPos (x - 2) (y + 1)
  BottomRight -> KnightPos (x + 2) (y + 1)
  LeftTop     -> KnightPos (x - 1) (y - 2)
  LeftBottom  -> KnightPos (x - 1) (y + 2)
  RightTop    -> KnightPos (x + 1) (y - 2)
  RightBottom -> KnightPos (x + 1) (y + 2)

-- Moves validations
isValidMove :: Board -> KnightPos -> KnightMove -> Bool
isValidMove (Board width height) (KnightPos x y) move = 
  let (KnightPos nx ny) = moveKnight (KnightPos x y) move
  in (nx >= 0) && (nx < width) && (ny >= 0) && (ny < height)

translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate _ [] = [] 
translate currentPos (move:moveList) = updatedPos : translate updatedPos moveList
  where updatedPos = moveKnight currentPos move

retraceMove :: KnightPos -> KnightPos -> Maybe KnightMove
retraceMove prevPos currPos = find ((== currPos) . moveKnight prevPos) allKnightMoves

translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' [] = Right []
translate' [_] = Right []
translate' (prevPos:currentPos:possionsList) = 
  case retraceMove prevPos currentPos of
    Nothing -> Left $ InvalidPosition currentPos
    Just move -> case translate' (currentPos:possionsList) of
        Left e -> Left e
        Right moves -> Right $ move : moves

-- tour driver code
tour :: Board -> KnightPos -> Maybe [KnightMove]
tour initialBoard initialPos = recTour initialBoard initialPos [initialPos] where
    nPositionsToVisit = width initialBoard * height initialBoard

    recTour :: Board -> KnightPos -> [KnightPos] -> Maybe [KnightMove]
    recTour _ _ visited | length visited == nPositionsToVisit = Just []
    recTour board pos visited = findValidPath allMoves
      where
        allMoves = [(move, newPos) | move <- allKnightMoves, let newPos = moveKnight pos move, isValidMove board pos move]

        findValidPath :: [(KnightMove, KnightPos)] -> Maybe [KnightMove]
        findValidPath [] = Nothing                                                -- There are No more moves to try
        findValidPath ((move, newPos) : restMoves)
            | newPos `elem` visited = findValidPath restMoves                     -- This square is already visited, try next move
            | otherwise = case recTour board newPos (newPos : visited) of
                Just movePath -> Just (move : movePath)                           -- Found a valid path, prepend the current move
                Nothing -> findValidPath restMoves                                -- There is No valid path from this move

-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark = undefined
