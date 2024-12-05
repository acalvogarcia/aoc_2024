import Data.List (elemIndex, sortBy)
import Split (splitBy, splitOnce)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day05/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day05/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day05/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day05/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getResultPart1
    getFileResultPart2 = getFileResult getResultPart2

getFileResult :: ([Update] -> [Rule] -> Int) -> String -> IO Int
getFileResult getResult file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let (rules, updates) = getRulesAndUpdates fileLines
  return $ getResult updates rules

getResultPart1 :: [Update] -> [Rule] -> Int
getResultPart1 updates rules = sum $ map getCenterValue validUpdates
  where
    validUpdates = filter (`followsAllRules` rules) updates

getResultPart2 :: [Update] -> [Rule] -> Int
getResultPart2 updates rules = sum $ map getCenterValue orderedUpdates
  where
    notvalidUpdates = filter (\u -> not $ u `followsAllRules` rules) updates
    orderedUpdates = map (reorderUpdate rules) notvalidUpdates

getRulesAndUpdates :: [String] -> ([Rule], [Update])
getRulesAndUpdates input = (rules, updates)
  where
    (ruleLines, updateLines) = splitOnce [""] [] input
    rules = map extractRule ruleLines
    updates = map extractUpdates updateLines

type Rule = (Int, Int)

extractRule :: String -> Rule
extractRule input = (read part1, read part2)
  where
    (part1, part2) = splitOnce "|" [] input

type Update = [Int]

extractUpdates :: String -> Update
extractUpdates input = map read $ splitBy "," [] input

followsRule :: Update -> Rule -> Bool
followsRule update (low, high) = case (maybeLowIdx, maybeHighIdx) of
  (Nothing, _) -> True
  (_, Nothing) -> True
  (Just lowIdx, Just highIdx) -> highIdx > lowIdx
  where
    maybeLowIdx = elemIndex low update
    maybeHighIdx = elemIndex high update

followsAllRules :: Update -> [Rule] -> Bool
followsAllRules update = all (followsRule update)

getCenterValue :: Update -> Int
getCenterValue update = update !! centerPosition
  where
    centerPosition = div (length update) 2

reorderUpdate :: [Rule] -> Update -> Update
reorderUpdate rules = sortBy (compareByRules rules)

compareByRules :: [Rule] -> Int -> Int -> Ordering
compareByRules [] _ _ = EQ
compareByRules (r : rs) a b
  | r == (a, b) = GT
  | r == (b, a) = LT
  | otherwise = compareByRules rs a b
