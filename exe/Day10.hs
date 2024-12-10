import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (ord)
import Data.List (elemIndices, group, sort)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day10/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day10/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day10/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day10/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getScoreMapFromPosition
    getFileResultPart2 = getFileResult getRatingsMapFromPosition

getFileResult :: ([[Int]] -> Position -> [[Maybe [a]]] -> [[Maybe [a]]]) -> String -> IO Int
getFileResult getHeadingsResults file = do
  fileHandle <- readFile file
  let input = map (map (\c -> ord c - 48)) $ lines fileHandle
  let headings = getHeadings input
  let fullScoreMap = getResultsFromHeadings getHeadingsResults input headings
  let result = sum $ map (\(x, y) -> maybe 0 length $ getElementInPosition fullScoreMap x y) headings
  return result

getResultsFromHeadings :: ([[Int]] -> Position -> [[Maybe [a]]] -> [[Maybe [a]]]) -> [[Int]] -> [Position] -> [[Maybe [a]]]
getResultsFromHeadings _ m [] = replicate h $ replicate l Nothing
  where
    (l, h) = getDimensions m
getResultsFromHeadings getResultsFromPosition heightsMap (p : ps) = thisScoreMap
  where
    othersScoreMap = getResultsFromHeadings getResultsFromPosition heightsMap ps
    thisScoreMap = getResultsFromPosition heightsMap p othersScoreMap

getScoreMapFromPosition :: [[Int]] -> Position -> ScoresMap -> ScoresMap
getScoreMapFromPosition heightsMap (x, y) m = newMap
  where
    newMap = swapElementInPosition newMapFromOthers (Just reachablePeaks) (x, y)
    currentHeight = getElementInPosition heightsMap x y
    positionsToCheck = getReachablePositions heightsMap (x, y)
    newMapFromOthers = foldr (getScoreMapFromPosition heightsMap) m positionsToCheck
    peaksFromOthers = map head $ group $ sort $ concatMap (extractResultsFromMap newMapFromOthers) positionsToCheck
    reachablePeaks
      | currentHeight == 9 = [(x, y)]
      | otherwise = peaksFromOthers

getRatingsMapFromPosition :: [[Int]] -> Position -> PathsMap -> PathsMap
getRatingsMapFromPosition heightsMap (x, y) m = newMap
  where
    newMap = swapElementInPosition newMapFromOthers (Just reachablePaths) (x, y)
    currentHeight = getElementInPosition heightsMap x y
    positionsToCheck = getReachablePositions heightsMap (x, y)
    newMapFromOthers = foldr (getRatingsMapFromPosition heightsMap) m positionsToCheck
    pathsFromOthers = map head $ group $ sort $ concatMap (extractResultsFromMap newMapFromOthers) positionsToCheck
    reachablePaths
      | currentHeight == 9 = [[(x, y)]]
      | otherwise = map (\p -> (x, y) : p) pathsFromOthers

extractResultsFromMap :: [[Maybe a]] -> Position -> a
extractResultsFromMap m (x, y) = case getElementInPosition m x y of
  Just a -> a
  Nothing -> error "Can't get reachable peaks before calculating it"

getReachablePositions :: [[Int]] -> Position -> [Position]
getReachablePositions heightsMap (x, y) = filter isReachable [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
  where
    currentHeight = getElementInPosition heightsMap x y
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions heightsMap
    isValidPosition (a, b) = 0 <= a && a <= maxX && 0 <= b && b <= maxY
    isReachable (a, b)
      | not $ isValidPosition (a, b) = False
      | getElementInPosition heightsMap a b /= currentHeight + 1 = False
      | otherwise = True

getHeadings :: [[Int]] -> [Position]
getHeadings m = concatMap headingsInRow $ enumerate m
  where
    headingsInRow :: (Int, [Int]) -> [Position]
    headingsInRow (y, r) = map (,y) $ elemIndices 0 r

type Position = (Int, Int)

type ScoresMap = [[Maybe [Position]]]

type PathsMap = [[Maybe [[Position]]]]

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

getDimensions :: [[a]] -> (Int, Int)
getDimensions m = (length $ head m, length m)

getElementInPosition :: [[a]] -> Int -> Int -> a
getElementInPosition m x y = m !! y !! x

swapElementInPosition :: [[a]] -> a -> Position -> [[a]]
swapElementInPosition m element (x, y) = newMatrix
  where
    originalRow = m !! y
    newRow = swapElementInList originalRow element x
    newMatrix = swapElementInList m newRow y

swapElementInList :: [a] -> a -> Int -> [a]
swapElementInList originalList element position = elementsBefore ++ element : elementsAfter
  where
    (elementsBefore, elementsFrom) = splitAt position originalList
    elementsAfter = tail elementsFrom
