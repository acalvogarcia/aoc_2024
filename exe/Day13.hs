import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (mapMaybe)
import Split (chunks, splitOnce)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day13/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day13/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day13/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day13/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getPrizeValuesPart1
    getFileResultPart2 = getFileResult getPrizeValuesPart2

getFileResult :: (String -> (Int, Int)) -> String -> IO Int
getFileResult getPrizeValues file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let equationDefinitions = map (getEquationParameters getPrizeValues . take 3) $ chunks 4 [] fileLines
  let equationSolutions = mapMaybe solveEquation equationDefinitions
  return $ sum $ map getCoins equationSolutions

getEquationParameters :: (String -> (Int, Int)) -> [String] -> EquationParameters
getEquationParameters getPrizeValues [buttonALine, buttonBLine, prizeLine] =
  EquationParameters {x = x, y = y, aX = aX, aY = aY, bX = bX, bY = bY}
  where
    (aX, aY) = getButtonValues buttonALine
    (bX, bY) = getButtonValues buttonBLine
    (x, y) = getPrizeValues prizeLine
getEquationParameters _ _ = error ""

getButtonValues :: String -> (Int, Int)
getButtonValues line = result
  where
    (_, valuesPart) = splitOnce "X+" [] line
    result = bimap read read $ splitOnce ", Y+" [] valuesPart

getPrizeValuesPart1 :: String -> (Int, Int)
getPrizeValuesPart1 line = result
  where
    (_, valuesPart) = splitOnce "X=" [] line
    result = bimap read read $ splitOnce ", Y=" [] valuesPart

getPrizeValuesPart2 :: String -> (Int, Int)
getPrizeValuesPart2 line = result
  where
    (_, valuesPart) = splitOnce "X=" [] line
    transform = (+) 10000000000000 . read
    result = bimap transform transform $ splitOnce ", Y=" [] valuesPart

getCoins :: EquationResult -> Int
getCoins EquationResult {a, b} = 3 * a + b

solveEquation :: EquationParameters -> Maybe EquationResult
solveEquation EquationParameters {x = x, y = y, aX = aX, aY = aY, bX = bX, bY = bY} =
  if aRemainder == 0 && bRemainder == 0
    then Just EquationResult {a, b}
    else Nothing
  where
    (a, aRemainder) = (x * bY - y * bX) `divMod` (aX * bY - aY * bX)
    (b, bRemainder) = (x * bY - aX * bY * a) `divMod` (bX * bY)

data EquationParameters = EquationParameters
  { x :: Int,
    y :: Int,
    aX :: Int,
    aY :: Int,
    bX :: Int,
    bY :: Int
  }

data EquationResult = EquationResult
  { a :: Int,
    b :: Int
  }
  deriving (Show)
