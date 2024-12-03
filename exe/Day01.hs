import Data.Array.Base (MArray (getNumElements))
import Data.Bifunctor (bimap)
import Data.List (sort, stripPrefix)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day01/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day01/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day01/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day01/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getPart1Result
    getFileResultPart2 = getFileResult getPart2Result

splitOnce :: (Eq a) => [a] -> [a] -> [a] -> ([a], [a])
splitOnce comp acc list =
  case stripPrefix comp list of
    Just rest -> (acc, rest)
    Nothing -> splitOnce comp (acc ++ take 1 list) (tail list)

getFileResult :: (([Int], [Int]) -> Int) -> String -> IO Int
getFileResult getResult file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let numberLists = getNumberLists fileLines [] []
  return $ getResult numberLists

getPart1Result :: ([Int], [Int]) -> Int
getPart1Result (l_1, l_2) = sum $ zipWith (\a b -> abs (a - b)) sorted_1 sorted_2
  where
    sorted_1 = sort l_1
    sorted_2 = sort l_2

getPart2Result :: ([Int], [Int]) -> Int
getPart2Result (l_1, l_2) = sum $ map (\n -> n * countOccurences n 0 l_2) l_1
  where
    countOccurences :: Int -> Int -> [Int] -> Int
    countOccurences comp acc (x : xs)
      | comp == x = countOccurences comp (acc + 1) xs
      | otherwise = countOccurences comp acc xs
    countOccurences _ acc [] = acc

getNumberLists :: [String] -> [Int] -> [Int] -> ([Int], [Int])
getNumberLists [] acc_1 acc_2 = (acc_1, acc_2)
getNumberLists (x : xs) acc_1 acc_2 = getNumberLists xs (n_1 : acc_1) (n_2 : acc_2)
  where
    (n_1, n_2) = bimap read read (splitOnce "   " [] x)
