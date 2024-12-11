import Data.List (find)
import Split (splitBy)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day11/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day11/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day11/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day11/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult 25
    getFileResultPart2 = getFileResult 75

getFileResult :: Int -> String -> IO Int
getFileResult numIterations file = do
  fileHandle <- readFile file
  let input :: [Int] = map read $ splitBy " " [] $ init fileHandle
  return $ getResult numIterations input

getResult :: Int -> [Int] -> Int
getResult numIterations input = sum $ map (getStoneValueAfterBlinksUnsafe finalMap numIterations) input
  where
    finalMap = foldr (updateMap numIterations) [] input

updateMap :: Int -> Int -> StonesAfterBlinksMap -> StonesAfterBlinksMap
updateMap 0 stone m = case getStoneValueAfterBlinks m 0 stone of
  Just _ -> m
  Nothing -> newMap
  where
    newMap = ((0, stone), 1) : m
updateMap blinks stone m = case getStoneValueAfterBlinks m blinks stone of
  Just _ -> m
  Nothing -> newMap
  where
    operation = getStoneOperation stone
    nextStones = case operation of
      Add -> [stone + 1]
      Multiply -> [2024 * stone]
      Divide -> divideStone stone
    mapAfterOthers = foldr (updateMap (blinks - 1)) m nextStones
    currentCount =
      sum $ map (getStoneValueAfterBlinksUnsafe mapAfterOthers (blinks - 1)) nextStones
    newMap = ((blinks, stone), currentCount) : mapAfterOthers

getStoneValueAfterBlinksUnsafe :: StonesAfterBlinksMap -> Int -> Int -> Int
getStoneValueAfterBlinksUnsafe m blinks stone =
  case getStoneValueAfterBlinks m blinks stone of
    Just n -> n
    Nothing -> error "Can't get number of stones before calculating it"

getStoneValueAfterBlinks :: StonesAfterBlinksMap -> Int -> Int -> Maybe Int
getStoneValueAfterBlinks m blinks stone =
  case element of
    Just (_, n) -> Just n
    Nothing -> Nothing
  where
    element = find (\((b, s), _) -> b == blinks && s == stone) m

type StonesAfterBlinksMap = [((Int, Int), Int)]

data StoneOperation = Add | Divide | Multiply

getStoneOperation :: Int -> StoneOperation
getStoneOperation s
  | s == 0 = Add
  | even numDigits = Divide
  | otherwise = Multiply
  where
    numDigits = length $ show s

divideStone :: Int -> [Int]
divideStone s = [read firstHalf, read secondHalf]
  where
    stoneString = show s
    stoneLength = length stoneString
    (firstHalf, secondHalf) = splitAt (stoneLength `div` 2) stoneString
