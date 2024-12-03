import Data.Bifunctor (bimap)
import Data.List (sort)
import Split (chunks, splitOnce)
import Text.Printf (printf)
import Text.Regex.TDFA

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day03/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day03/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day03/test-input-2.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day03/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getLineResultPart1
    getFileResultPart2 = getFileResult getLineResultPart2

getFileResult :: (String -> Int) -> String -> IO Int
getFileResult getLineResult file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let result = sum $ map getLineResult fileLines
  return result

getLineResultPart1 :: String -> Int
getLineResultPart1 input = sum $ map getMulResult allMatches
  where
    allMatches = getAllTextMatches (input =~ mulRegex) :: [String]
    mulRegex = "mul\\(([0-9]+),([0-9]+)\\)"

getLineResultPart2 :: String -> Int
getLineResultPart2 input = sum $ map (getSubResult input) subStringPositions
  where
    doRegex = "do\\(\\)"
    dontRegex = "don't\\(\\)"
    doIndices = (0, 0) : getAllMatches (input =~ doRegex) :: [(Int, Int)]
    dontIndices = getAllMatches (input =~ dontRegex) :: [(Int, Int)]
    rawDoOrDonts = map (Do . fst) doIndices ++ map (Dont . fst) dontIndices
    doOrDonts = simplifyDoOrDonts (sort rawDoOrDonts) []
    pairs = map (map getDoOrDontIndex) $ chunks 2 [] doOrDonts
    subStringPositions = map (\c -> if length c == 1 then (head c, Nothing) else (head c, Just (last c))) pairs

getSubResult :: String -> (Int, Maybe Int) -> Int
getSubResult input (start, maybe_end) = getLineResultPart1 subString
  where
    end = case maybe_end of
      Just n -> n
      Nothing -> length input
    subString = take (end - start) $ drop start input

data DoOrDont = Do Int | Dont Int deriving (Show)

instance Eq DoOrDont where
  a == b = getDoOrDontIndex a == getDoOrDontIndex b

instance Ord DoOrDont where
  compare a b = compare (getDoOrDontIndex a) (getDoOrDontIndex b)

getDoOrDontIndex :: DoOrDont -> Int
getDoOrDontIndex (Do n) = n
getDoOrDontIndex (Dont n) = n

simplifyDoOrDonts :: [DoOrDont] -> [DoOrDont] -> [DoOrDont]
simplifyDoOrDonts [] acc = acc
simplifyDoOrDonts (x : xs) [] = case x of
  Do _ -> simplifyDoOrDonts xs [x]
  Dont _ -> simplifyDoOrDonts xs []
simplifyDoOrDonts (x : xs) acc = case x of
  Do _ -> case last acc of
    Do _ -> simplifyDoOrDonts xs acc
    Dont _ -> simplifyDoOrDonts xs (acc ++ [x])
  Dont _ -> case last acc of
    Dont _ -> simplifyDoOrDonts xs acc
    Do _ -> simplifyDoOrDonts xs (acc ++ [x])

getMulResult :: String -> Int
getMulResult input = n_1 * n_2
  where
    getNumbersStr = init . drop 4
    (n_1, n_2) = bimap read read (splitOnce "," [] $ getNumbersStr input)
