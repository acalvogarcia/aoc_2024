import Split (splitBy, splitOnce)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day07/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day07/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day07/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day07/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult [(+), (*)]
    getFileResultPart2 = getFileResult [(+), (*), combine]

getFileResult :: [Int -> Int -> Int] -> String -> IO Int
getFileResult operators file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let equations = map getResultAndValues fileLines
  -- let operators :: [Operator] = [(+), (*)]
  let allResults = map (\(result, values) -> (result, isResultValid result values operators)) equations
  let validResults = filter snd allResults
  return $ sum $ map fst validResults

getResultAndValues :: String -> (Int, [Int])
getResultAndValues input = (read resultString, values)
  where
    (resultString, valuesString) = splitOnce ": " [] input
    values = map read $ splitBy " " [] valuesString

type Operator = Int -> Int -> Int

isResultValid :: Int -> [Int] -> [Operator] -> Bool
isResultValid result values operators = anyValid
  where
    allCombinations = getAllCombinations operators [] (length values - 1)
    anyValid = any (\os -> result == getResult Nothing os values) allCombinations

getAllCombinations :: [a] -> [[a]] -> Int -> [[a]]
getAllCombinations elements [] 1 = map (: []) elements
getAllCombinations elements [] numOperations = getAllCombinations elements (map (: []) elements) (numOperations - 1)
getAllCombinations elements acc numOperations
  | numOperations == 1 = newAcc
  | otherwise = getAllCombinations elements newAcc (numOperations - 1)
  where
    newAcc = [x : y | x <- elements, y <- acc]

getResult :: Maybe Int -> [Operator] -> [Int] -> Int
getResult Nothing _ [value] = value
getResult Nothing _ [] = error "No values found"
getResult Nothing [] _ = error "No operators found"
getResult Nothing (operator : os) (v1 : v2 : values) = getResult newValue os values
  where
    newValue = Just (operator v1 v2)
getResult (Just value) _ [] = value
getResult (Just acc) (operator : os) (value : vs) = getResult newValue os vs
  where
    newValue = Just (operator acc value)
getResult (Just _) [] _ = error "Not enough operators found"

combine :: Int -> Int -> Int
combine left right = read (leftString ++ rightString)
  where
    leftString = show left
    rightString = show right
