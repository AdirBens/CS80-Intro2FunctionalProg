-- HW2Test.hs - Test suite for HW2.hs
-- To run execute:
-- 1. ghci HW2Test.hs
-- 2. main

import Distribution.InstalledPackageInfo qualified as HW2test
import HW2

-- General check function that returns a tuple with a Bool for success and a message for failure
check :: (Eq a, Show a) => String -> a -> a -> (Bool, String)
check testName output expected =
  if output == expected
    then (True, "")
    else (False, testName ++ " Failed: Expected " ++ show expected ++ ", but got " ++ show output)

valid8Tour = Just [TopLeft, RightTop, BottomRight, LeftBottom, BottomLeft, TopLeft, RightTop, BottomRight, LeftBottom, TopLeft, TopRight, BottomRight, LeftBottom, BottomLeft, TopLeft, RightTop, BottomRight, LeftBottom, TopLeft, TopRight, BottomLeft, TopLeft, RightTop, BottomRight, LeftBottom, TopLeft, TopRight, BottomRight, LeftBottom, BottomLeft, TopLeft, RightTop,  RightTop, BottomRight, LeftBottom, TopLeft, TopRight, BottomLeft, TopLeft, RightTop, BottomRight, LeftBottom, TopLeft, TopRight, LeftBottom, BottomLeft, TopLeft, RightTop, BottomLeft, TopLeft, RightTop, BottomRight, TopLeft, RightTop, BottomRight, LeftBottom, BottomLeft, TopLeft, RightTop, BottomRight, LeftBottom, BottomLeft, TopLeft, RightTop]

-- List of all test cases, structured for easy extension
testCases :: [IO (Bool, String)]
testCases =
  [ -- Tests for concatMaybeMap
    return $ check "concatMaybeMap Test 1" (concatMaybeMap (\x -> if x > 0 then Just $ x * 10 else Nothing) Nothing) Nothing,
    return $ check "concatMaybeMap Test 2" (concatMaybeMap (\x -> if x > 0 then Just $ x * 10 else Nothing) (Just 10)) (Just 100),
    return $ check "concatMaybeMap Test 3" (concatMaybeMap (\x -> if x > 0 then Just $ x * 10 else Nothing) (Just (-10))) Nothing,
    -- Tests for fromMaybe
    return $ check "fromMaybe Test 1" (fromMaybe 1 Nothing) 1,
    return $ check "fromMaybe Test 2" (fromMaybe 1 (Just 2)) 2,
    -- Tests for maybe function, adding type annotation for length
    return $ check "maybe Test 1" (HW2.maybe 1 (length :: String -> Int) Nothing) 1,
    return $ check "maybe Test 2" (HW2.maybe 1 (length :: String -> Int) (Just "foo")) 3,
    -- Tests for catMaybes
    return $ check "catMaybes Test 1" (catMaybes [Just 1, Nothing, Just 3]) [1, 3],
    return $ check "catMaybes Test 3" (catMaybes [Just 1, Just 2, Just 3]) [1, 2, 3],
    -- Tests for mapMaybe
    return $ check "mapMaybe Test" (mapMaybe (\x -> if x > 0 then Just $ x * 10 else Nothing) [1, -1, 10]) [10, 100],
    -- Tests for either
    return $ check "either Test 1" (HW2.either (length :: String -> Int) (* 10) (Left "foo")) 3,
    return $ check "either Test 2" (HW2.either (length :: String -> Int) (* 10) (Right 10)) 100,
    -- Tests for mapLeft
    return $ check "mapLeft Test 1" (mapLeft (++ " bar ") (Left "foo" :: Either String Int)) (Left "foo bar " :: Either String Int),
    return $ check "mapLeft Test 2" (mapLeft (++ " bar ") (Right 10 :: Either String Int)) (Right 10 :: Either String Int),
    -- Tests for catEithers
    return $ check "catEithers Test 1" (catEithers [Right 10, Right 20] :: Either String [Int]) (Right [10, 20] :: Either String [Int]),
    return $ check "catEithers Test 2" (catEithers [Right 10, Left "foo", Right 20, Left "bar"] :: Either String [Int]) (Left "foo" :: Either String [Int]),
    -- Tests for mapEither
    return $ check "mapEither Test 1" (mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, 2, 3]) (Right [10, 20, 30]),
    return $ check "mapEither Test 2" (mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, -1, 2, -2]) (Left 4),
    -- Tests for concatEitherMap
    return $ check "concatEitherMap Test 1" (concatEitherMap (Right . (* 10)) (Right 5 :: Either Int Int)) (Right 50 :: Either Int Int),
    return $ check "concatEitherMap Test 2" (concatEitherMap (Right . (* 10)) (Left 5 :: Either Int Int)) (Left 5 :: Either Int Int),
    -- Tests for partitionEithers
    return $ check "partitionEithers Test" (partitionEithers [Right "foo ", Left 42, Right "bar", Left 54]) ([42, 54], ["foo ", "bar"]),
    -- Test for eitherToMaybe
    return $ check "eitherToMaybe Test 1" (eitherToMaybe (Right 10 :: Either String Int)) (Just 10),
    return $ check "eitherToMaybe Test 2" (eitherToMaybe (Left "foo" :: Either String Int)) Nothing,
    -- Tests for take
    return $ check "take Test 1" (HW2.take 0 [1, 2, 3]) ([] :: [Int]),
    return $ check "take Test 2" (HW2.take 2 [1, 2, 3]) ([1, 2] :: [Int]),
    return $ check "take Test 3" (HW2.take 4 [1, 2, 3]) ([1, 2, 3] :: [Int]),
    return $ check "take Test 4" (HW2.take 4 [1 ..]) ([1, 2, 3, 4] :: [Int]),
    -- Tests for drop
    return $ check "drop Test 1" (HW2.drop 0 [1, 2, 3]) ([1, 2, 3] :: [Int]),
    return $ check "drop Test 2" (HW2.drop 2 [1, 2, 3]) ([3] :: [Int]),
    return $ check "drop Test 3" (HW2.drop 4 [1, 2, 3]) ([] :: [Int]),
    return $ check "drop Test 4" (HW2.take 5 $ HW2.drop 4 [1 ..]) ([5, 6, 7, 8, 9] :: [Int]),
    -- Tests for takeWhile
    return $ check "takeWhile Test 1" (HW2.takeWhile (< 1) [1 ..]) ([] :: [Int]),
    return $ check "takeWhile Test 2" (HW2.takeWhile (< 5) [1 ..]) ([1, 2, 3, 4] :: [Int]),
    -- Tests for dropWhile
    return $ check "dropWhile Test 1" (HW2.take 5 $ HW2.dropWhile (< 1) [1 ..]) ([1, 2, 3, 4, 5] :: [Int]),
    return $ check "dropWhile Test 2" (HW2.take 5 $ HW2.dropWhile (< 5) [1 ..]) ([5, 6, 7, 8, 9] :: [Int]),
    -- Tests for reverse
    return $ check "reverse Test 1" (HW2.reverse []) ([] :: [Int]),
    return $ check "reverse Test 2" (HW2.reverse [1, 2, 3]) ([3, 2, 1] :: [Int]),
    -- Tests for rotate
    return $ check "rotate Test 1" (rotate (-1) [1, 2, 3]) ([1, 2, 3] :: [Int]),
    return $ check "rotate Test 2" (rotate 0 [1, 2, 3]) ([1, 2, 3] :: [Int]),
    return $ check "rotate Test 3" (rotate 2 [1, 2, 3]) ([2, 3, 1] :: [Int]),
    return $ check "rotate Test 4" (rotate 5 [1, 2, 3]) ([2, 3, 1] :: [Int]),
    -- Tests for lotate
    return $ check "lotate Test 1" (lotate (-1) [1, 2, 3]) ([1, 2, 3] :: [Int]),
    return $ check "lotate Test 2" (lotate 0 [1, 2, 3]) ([1, 2, 3] :: [Int]),
    return $ check "lotate Test 3" (lotate 2 [1, 2, 3]) ([3, 1, 2] :: [Int]),
    return $ check "lotate Test 4" (lotate 5 [1, 2, 3]) ([3, 1, 2] :: [Int]),
    -- Tests for fromGenerator
    return $ check "fromGenerator Test 1" (fromGenerator ((+ 1), (< 0), 0)) ([] :: [Int]),
    return $ check "fromGenerator Test 2" (fromGenerator ((+ 1), (<= 0), 0)) ([1] :: [Int]),
    return $ check "fromGenerator Test 3" (fromGenerator ((+ 1), (< 5), 0)) ([1, 2, 3, 4, 5] :: [Int]),
    return $ check "fromGenerator Test 4" (HW2.take 5 $ fromGenerator ((+ 1), const True, 0)) ([1, 2, 3, 4, 5] :: [Int]),
    -- Tests for replicate
    return $ check "replicate Test 1" (HW2.replicate 0 42) ([] :: [Int]),
    return $ check "replicate Test 2" (HW2.replicate 3 42) ([42, 42, 42] :: [Int]),
    -- Tests for inits
    return $ check "inits Test 1" (inits [1, 2, 3]) ([[], [1], [1, 2], [1, 2, 3]] :: [[Int]]),
    return $ check "inits Test 2" (HW2.take 5 $ inits [1 ..]) ([[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4]] :: [[Int]]),
    -- Tests for tails
    return $ check "tails Test 1" (tails [1, 2, 3]) ([[1, 2, 3], [2, 3], [3], []] :: [[Int]]),
    return $ check "map/tails Test 2" (map (HW2.take 3) $ HW2.take 5 $ tails [1 ..]) ([[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6], [5, 6, 7]] :: [[Int]]),
    -- Tests for zipWith
    return $ check "zipWith Test 1" (HW2.zipWith (+) [1, 2, 3] [4, 5, 6]) ([5, 7, 9] :: [Int]),
    return $ check "zipWith Test 2" (HW2.zipWith (+) [1, 2] [4, 5, 6]) ([5, 7] :: [Int]),
    -- Tests for zip
    return $ check "zip Test 1" (HW2.zip [1, 2] [4, 5, 6]) ([(1, 4), (2, 5)] :: [(Int, Int)]),
    return $ check "zip Test 2" (HW2.zip [1, 2, 3] [4, 5, 6]) ([(1, 4), (2, 5), (3, 6)] :: [(Int, Int)]),
    -- Tests for zipFill
    return $ check "zipFill Test 1" (zipFill 0 'a' [1, 2, 3] "foobar") ([(1, 'f'), (2, 'o'), (3, 'o'), (0, 'b'), (0, 'a'), (0, 'r')] :: [(Int, Char)]),
    return $ check "zipFill Test 2" (zipFill 0 'a' [1 .. 6] "foo") ([(1, 'f'), (2, 'o'), (3, 'o'), (4, 'a'), (5, 'a'), (6, 'a')] :: [(Int, Char)]),
    return $ check "zipFill Test 3" (HW2.take 10 $ zipFill 1 'a' [1 ..] "foo") ([(1, 'f'), (2, 'o'), (3, 'o'), (4, 'a'), (5, 'a'), (6, 'a'), (7, 'a'), (8, 'a'), (9, 'a'), (10, 'a')] :: [(Int, Char)]),
    -- Tests for zipFail
    return $ check "zipFail Test 1" (zipFail [1, 2] "foobar") (Left ErrorFirst :: Either ZipFail [(Int, Char)]),
    return $ check "zipFail Test 2" (zipFail [1 ..] "foobar") (Left ErrorSecond :: Either ZipFail [(Int, Char)]),
    return $ check "zipFail Test 3" (zipFail [1, 2, 3] "foo") (Right [(1, 'f'), (2, 'o'), (3, 'o')] :: Either ZipFail [(Int, Char)]),

    -- Tests for translate
    return $ check "translate Test 1" (HW2.translate (KnightPos (-2) (-1)) [TopLeft, TopRight]) [KnightPos (-4) (-2), KnightPos (-2) (-3)],
    return $ check "translate Test 2" (HW2.translate (KnightPos 2 3) [RightTop, BottomRight]) [KnightPos 3 1, KnightPos 5 2],  -- Two valid moves
    return $ check "translate Test 3" (HW2.translate (KnightPos 0 0) []) [], -- No moves

    -- Tests for translate'
    return $ check "translate' Test 1" (HW2.translate' [ KnightPos 0 0, KnightPos 1 2]) (Right [ RightBottom ]),
    return $ check "translate' Test 2" (HW2.translate' [ KnightPos 0 0, KnightPos 1 2, KnightPos 0 0, KnightPos 2 1]) (Right [ RightBottom , LeftTop , BottomRight ]),
    return $ check "translate' Test 3" (HW2.translate' [ KnightPos 0 0, KnightPos ( -1) ( -2)]) (Right [ LeftTop ]),
    return $ check "translate' Test 4" (HW2.translate' [ KnightPos 0 0, KnightPos 2 2]) (Left ( InvalidPosition (KnightPos { x = 2, y = 2 }))),

    -- Tests for tour
    return $ check "tour Test 0 (from pdf)" (HW2.tour (Board 5 5) (KnightPos 0 0)) (Just [RightBottom , TopRight , TopLeft , RightBottom , BottomRight , BottomLeft , TopLeft , RightTop , TopRight , RightBottom , LeftBottom , TopLeft , LeftTop , TopRight , BottomRight , LeftBottom , BottomLeft , LeftTop , TopRight , TopRight , LeftBottom , RightBottom , TopLeft , BottomLeft]),  -- Complete tour on 5x5 board
    return $ check "tour Test 5" (HW2.tour (Board 1 1) (KnightPos 0 0)) (Just []),  -- standing knight
    return $ check "tour Test 2" (HW2.tour (Board 4 4) (KnightPos 0 0)) Nothing,  -- No tour possible if starting position doesn't allow full coverage
    return $ check "tour Test 3" (HW2.tour (Board 3 3) (KnightPos 0 0)) Nothing,  -- No tour possible on a smaller board (3x3)
    return $ check "tour Test 4" (HW2.tour (Board 6 6) (KnightPos 0 0)) (Just [RightBottom,TopRight,TopLeft,RightBottom,TopLeft,TopRight,RightBottom,TopLeft,LeftBottom,RightBottom,RightTop,BottomLeft,BottomRight,TopRight,RightTop,LeftTop,BottomLeft,BottomLeft,RightBottom,BottomRight,TopRight,TopLeft,RightTop,RightBottom,LeftBottom,TopLeft,BottomLeft,RightTop,BottomRight,BottomRight,LeftTop,RightTop,TopLeft,RightBottom,RightTop]) -- board (6x6)
    -- return $ check "tour Test 4" (HW2.tour (Board 8 8) (KnightPos 0 0)) (valid8Tour)  --  8x8 board
  ]

-- Main function to run all tests and print results selectively
main :: IO ()
main = do
  putStrLn "Running tests for HW2.hs..."
  results <- sequence testCases
  let failures = filter (not . fst) results
  if null failures
    then putStrLn "All tests passed. Congratulations!"
    else mapM_ (putStrLn . snd) failures
  putStrLn "Testing complete."