{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
import Data.List (stripPrefix)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day02/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day02/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day02/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day02/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getPart1Result
    getFileResultPart2 = getFileResult getPart2Result

splitBy :: (Eq a) => [a] -> [a] -> [a] -> [[a]]
splitBy _ acc [] = [acc]
splitBy comp acc list =
  case stripPrefix comp list of
    Just _ -> acc : splitBy comp [] (drop (length comp) list)
    Nothing -> splitBy comp (acc ++ take 1 list) (tail list)

getFileResult :: ([[Int]] -> Int) -> String -> IO Int
getFileResult getResult file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let rows :: [[Int]] = map (map read . splitBy " " []) fileLines
  return $ getResult rows

getPart1Result :: [[Int]] -> Int
getPart1Result ls = length $ filter (`isSafe` Undefined) ls

getPart2Result :: [[Int]] -> Int
getPart2Result ls = length $ filter (\l -> isSafeWithDampener [] l Undefined) ls

data IncDec = Inc | Dec | Undefined

isSafe :: [Int] -> IncDec -> Bool
isSafe [] _ = True
isSafe [_] _ = True
isSafe (x : y : xs) inc_dec
  | x == y = False
  | abs (y - x) > 3 = False
  | otherwise = case inc_dec of
      Undefined -> isSafe (y : xs) (if y > x then Inc else Dec)
      Inc -> (y > x) && isSafe (y : xs) Inc
      Dec -> (y < x) && isSafe (y : xs) Dec

isSafeWithDampener :: [Int] -> [Int] -> IncDec -> Bool
isSafeWithDampener _ [] _ = True
isSafeWithDampener _ [_] _ = True
isSafeWithDampener acc (x : y : xs) inc_dec
  | x == y = isSublistSafe
  | abs (y - x) > 3 = isSublistSafe
  | otherwise = case inc_dec of
      Undefined -> isSafeWithDampener (acc ++ [x]) (y : xs) (if y > x then Inc else Dec)
      Inc -> case y > x of
        True -> isSafeWithDampener (acc ++ [x]) (y : xs) Inc
        False -> isSublistSafe
      Dec -> case y < x of
        True -> isSafeWithDampener (acc ++ [x]) (y : xs) Dec
        False -> isSublistSafe
  where
    isSublistSafe = isSafe (acc ++ [x] ++ xs) Undefined || isSafe (acc ++ [y] ++ xs) Undefined || not (null acc) && isSafe (init acc ++ [x, y] ++ xs) Undefined
