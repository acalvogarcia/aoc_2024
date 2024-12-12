import Data.Bifunctor (bimap)
import Data.List (groupBy, sortBy)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day12/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day12/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day12/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day12/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getRegionFences
    getFileResultPart2 = getFileResult getRegionSides

getFileResult :: ([Position] -> Int) -> String -> IO Int
getFileResult getSides file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let (l, h) = getDimensions fileLines
  let allPositions = [(x, y) | x <- [0 .. l - 1], y <- [0 .. h - 1]]
  let (_, allRegions) = foldr (addNewRegion fileLines) ([], []) allPositions
  return $ sum $ map (\r -> length r * getSides r) allRegions

getRegionFences :: [Position] -> Int
getRegionFences positions = sum $ map (getPositionFences positions) positions

getRegionSides :: [Position] -> Int
getRegionSides positions = horizontalLines + verticalLines
  where
    groupedHorizontally = groupBy (\(_, a) (_, b) -> a == b) $ sortBy (\(a, b) (c, d) -> compare (b, a) (d, c)) positions
    horizontalLines = sum $ map (\ps -> getLineSides positions ps (Nothing, Nothing)) groupedHorizontally
    positionsInverted = map (\(a, b) -> (b, a)) positions
    groupedVertically = groupBy (\(_, a) (_, b) -> a == b) $ sortBy (\(a, b) (c, d) -> compare (b, a) (d, c)) positionsInverted
    verticalLines = sum $ map (\ps -> getLineSides positionsInverted ps (Nothing, Nothing)) groupedVertically

getLineSides :: [Position] -> [Position] -> (Maybe Int, Maybe Int) -> Int
getLineSides _ [] _ = 0
getLineSides region ((x, y) : ps) (prevNotCovered, prevNotUndercovered) = fromEnum addSideBelow + fromEnum addSideAbove + getLineSides region ps (newPrevCovered, newPrevUndercovered)
  where
    isNotCovered = (x, y - 1) `notElem` region
    isNotUndercovered = (x, y + 1) `notElem` region
    addSideAbove = isNotCovered && (prevNotCovered /= Just (x - 1))
    addSideBelow = isNotUndercovered && (prevNotUndercovered /= Just (x - 1))
    newPrevCovered = if isNotCovered then Just x else prevNotCovered
    newPrevUndercovered = if isNotUndercovered then Just x else prevNotUndercovered

getPositionFences :: [Position] -> Position -> Int
getPositionFences region (x, y) = length neighboursOutsideRegion
  where
    neighboursOutsideRegion =
      filter
        (`notElem` region)
        [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

addNewRegion :: [[Char]] -> Position -> ([Position], [[Position]]) -> ([Position], [[Position]])
addNewRegion charsMap pos (alreadyChecked, existingRegions)
  | pos `elem` alreadyChecked = (alreadyChecked, existingRegions)
  | otherwise = (newAlreadyChecked, newRegion : existingRegions)
  where
    (newAlreadyChecked, newRegion) = getRegionFromPosition charsMap pos (alreadyChecked, [])

getRegionFromPosition :: [[Char]] -> Position -> ([Position], [Position]) -> ([Position], [Position])
getRegionFromPosition charsMap (x, y) (alreadyChecked, acc)
  | (x, y) `elem` alreadyChecked = (alreadyChecked, acc)
  | otherwise = (newChecked, newFound)
  where
    currentChar = getElementInPosition charsMap x y
    (newChecked, newFound) =
      foldr
        (getRegionFromPosition charsMap)
        ((x, y) : alreadyChecked, (x, y) : acc)
        inSameRegion
    inSameRegion =
      filter
        ( \(a, b) ->
            isValidPosition (a, b)
              && currentChar == getElementInPosition charsMap a b
              && notElem (a, b) alreadyChecked
        )
        [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions charsMap
    isValidPosition (a, b) = 0 <= a && a <= maxX && 0 <= b && b <= maxY

type Position = (Int, Int)

getDimensions :: [[a]] -> (Int, Int)
getDimensions m = (length $ head m, length m)

getElementInPosition :: [[a]] -> Int -> Int -> a
getElementInPosition m x y = m !! y !! x
