-- HW3Test.hs - Test suite for HW3.hs
-- To run execute:
-- 1. ghci HW3Test.hs
-- 2. main

-- Assuming serialization and deserialization functions are defined here

import Data.Char (chr, ord, toLower, toUpper)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Deque
import HW5

-- General check function that returns a tuple with a Bool for success and a message for failure
check :: (Eq a, Show a) => String -> a -> a -> (Bool, String)
check testName output expected =
  if output == expected
    then (True, "")
    else (False, testName ++ " Failed: Expected " ++ show expected ++ ", but got " ++ show output)

q1 = Deque.pushl 1 $ Deque.pushl 2 $ Deque.pushr 3 Deque.empty

q2 = Deque.pushr 30 $ Deque.pushr 20 $ Deque.pushl 10 Deque.empty

variables = M.fromList [("x", 10), ("y", 20)]

exprs1 = [Val 1, Add (Var "x") (Val 2), Div (Val 30) (Var "y"), Mul (Var "x") (Var "y")]

exprs2 = [Val 1, Add (Var "z") (Val 2), Div (Var "y") (Val 0)]

defaults =
  Defaults
    { defaultForDivisionByZero = 0,
      defaultForVariable = sum . map ord
    }

go :: (CalculatorError f) => [Expr] -> f [Int]
go = traverse $ runCalculator variables


-- List of all test cases, structured for easy extension
testCases :: [IO (Bool, String)]
testCases =
  [ return $ check "sum test" (foldMap' fmsum [1, 2, 3]) 6,
    return $ check "or test 1" (foldMap' fmor [False, True, False]) True,
    return $ check "or test 2" (foldMap' fmor [False, False, False]) False,
    return $ check "fold test" (foldMap' fmfold $ map show [1, 2, 3]) "123",
    return $ check "elem test" (foldMap' (fmelem 2) [1, 2, 3]) True,
    return $ check "find test 1" (foldMap' (fmfind (> 2)) [1, 2, 3]) (Just 3),
    return $ check "find test 2" (foldMap' (fmfind (> 3)) [1, 2, 3]) Nothing,
    return $ check "max test 1" (foldMap'  fmmaximum [1, 2, 3]) (Just 3),
    return $ check "min test 1" (foldMap'  fmminimum [1, 2, 3]) (Just 1),
    return $ check "maxby test 1" (foldMap' (fmmaxBy length) ["foo", "bar", "bazz"]) (Just "bazz"),
    return $ check "maxby test 1" (foldMap' (fmminBy length) ["foo", "bar", "bazz", "ci"]) (Just "ci"),
    return $ check "length test" (foldMap' fmlength [1, 2, 3]) 3,
    return $ check "null test 1" (foldMap' fmnull [1, 2, 3]) False,
    return $ check "null test 2" (foldMap' fmnull []) True,
    return $ check "toList test 1" (foldMap' fmtoList [1, 2, 3, 4, 5]) [1, 2, 3, 4, 5],
    return $ check "toList test 2" (foldMap' fmtoList "Hello") "Hello",
    return $ check "toList test 3" (foldMap' fmtoList $ 1 :| [2, 3]) [1, 2, 3],
    return $ check "test calc with maybe 1" (go exprs1 :: Maybe [Int]) (Just [1, 12, 1, 200]),
    return $ check "test calc with maybe 2" (go exprs2 :: Maybe [Int]) Nothing,
    return $ check "test calc with either 1" (go exprs1 :: Either Err [Int]) (Right [1, 12, 1, 200]),
    return $ check "test calc with either 2" (go exprs2 :: Either Err [Int]) (Left (MissingVar "z")),
    return $ check "test calc with reader 1" (runReader (go exprs1) defaults) [1, 12, 1, 200],
    return $ check "test calc with reader 2" (runReader (go exprs2) defaults) [1, 124, 0]
  ]

-- Main function to run all tests and print results selectively
main :: IO ()
main = do
  putStrLn "Running tests for HW5.hs..."
  results <- sequence testCases
  let failures = filter (not . fst) results
  if null failures
    then putStrLn "All tests passed. Congratulations!"
    else mapM_ (putStrLn . snd) failures
  putStrLn "Testing complete."
