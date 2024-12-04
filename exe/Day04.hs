import Data.Bifunctor (Bifunctor (bimap))
import Text.Printf (printf)
import Text.Regex.TDFA

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day04/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day04/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day04/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day04/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getResultPart1
    getFileResultPart2 = getFileResult getResultPart2

getFileResult :: ([String] -> Int) -> String -> IO Int
getFileResult getResult file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  return $ getResult fileLines

getElementInPosition :: [[a]] -> Int -> Int -> a
getElementInPosition m x y = m !! y !! x

getDimensions :: [[a]] -> (Int, Int)
getDimensions m = (length $ head m, length m)

transpose :: [[a]] -> [[a]]
transpose m = map (\x -> map (getElementInPosition m x) [0 .. maxY]) [0 .. maxX]
  where
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m

rotate :: [[a]] -> [[a]]
rotate m = reverse $ transpose m

getAllDiagonals :: [[a]] -> [[a]]
getAllDiagonals m = mainDiagonal : lowerDiagonals ++ upperDiagonals
  where
    mainDiagonal = zipWith (!!) m [0 .. (min maxX maxY)]
    lowerDiagonals = getLowerDiagonals m
    upperDiagonals = getUpperDiagonals m
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m

getLowerDiagonals :: [[a]] -> [[a]]
getLowerDiagonals m = map getLowerDiagonalN [1 .. (min maxX maxY)]
  where
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m
    getLowerDiagonalN n = map (\x -> getElementInPosition m x (x + n)) [0 .. (min maxX (maxY - n))]

getUpperDiagonals :: [[a]] -> [[a]]
getUpperDiagonals m = map getUpperDiagonalN [1 .. (min maxX maxY)]
  where
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m
    getUpperDiagonalN n = map (\x -> getElementInPosition m (x + n) x) [0 .. (min maxX (maxY - n))]

getResultPart1 :: [String] -> Int
getResultPart1 horizontalLines = horizontalLinesResult + verticalLinesResult + allDiagonalsResult
  where
    verticalLines = transpose horizontalLines
    allDiagonals = getAllDiagonals horizontalLines ++ getAllDiagonals (rotate horizontalLines)
    horizontalLinesResult = sum $ map getLineResultRegex horizontalLines
    verticalLinesResult = sum $ map getLineResultRegex verticalLines
    allDiagonalsResult = sum $ map getLineResultRegex allDiagonals

getLineResultRegex :: String -> Int
getLineResultRegex input = length allMatches + length allMatchesInverted
  where
    allMatches = getAllTextMatches (input =~ "XMAS") :: [String]
    allMatchesInverted = getAllTextMatches (input =~ "SAMX") :: [String]

getResultPart2 :: [String] -> Int
getResultPart2 m = sum $ map (length . filter isCrossXMas) allCrosses
  where
    allCrosses = map (\x -> map (getCrossesFromPosition x) [0 .. maxY - 2]) [0 .. maxX - 2]
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m
    getCrossesFromPosition x y = ((getCharacterInPosition x y, getCharacterInPosition (x + 1) y, getCharacterInPosition (x + 2) y), (getCharacterInPosition x (y + 1), getCharacterInPosition (x + 1) (y + 1), getCharacterInPosition (x + 2) (y + 1)), (getCharacterInPosition x (y + 2), getCharacterInPosition (x + 1) (y + 2), getCharacterInPosition (x + 2) (y + 2)))
    getCharacterInPosition = getElementInPosition m

type CrossLine = (Char, Char, Char)

type Cross = (CrossLine, CrossLine, CrossLine)

isCrossXMas :: Cross -> Bool
isCrossXMas ((a11, _, a13), (_, a22, _), (a31, _, a33)) = firstMas && secondMas
  where
    firstDiag = [a11, a22, a33]
    secondDiag = [a13, a22, a31]
    firstMas = firstDiag == "MAS" || firstDiag == "SAM"
    secondMas = secondDiag == "MAS" || secondDiag == "SAM"
