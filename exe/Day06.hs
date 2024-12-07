import Data.Bifunctor (bimap)
import Data.List (findIndex, group, sort)
import Data.Maybe (isJust)
import Text.Printf (printf)
import Prelude hiding (Left, Right)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day06/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day06/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day06/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day06/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getResultPart1
    getFileResultPart2 = getFileResult getResultPart2

getFileResult :: ([[Char]] -> Int) -> String -> IO Int
getFileResult getResult file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  return $ getResult fileLines

getResultPart1 :: [[Char]] -> Int
getResultPart1 m = result
  where
    (initialPosition, initialDirection) = findGuardInitialStep m
    allTravelledPositions = getPositions $ getTravelledRoute m initialPosition initialDirection []
    result = length $ group $ sort allTravelledPositions

getResultPart2 :: [[Char]] -> Int
getResultPart2 m = length $ filter loops newRoutesNew
  where
    (initialPosition, initialDirection) = findGuardInitialStep m
    originalRoute = getTravelledRoute m initialPosition initialDirection []
    positionsToCheck = map head $ group $ sort $ init $ getPositions originalRoute
    newMaps = map (swapElementInPosition m '#') positionsToCheck
    newRoutesNew = map (\ma -> getTravelledRoute ma initialPosition initialDirection []) newMaps

type Step = (Position, Direction)

type Position = (Int, Int)

data Direction = Up | Down | Left | Right deriving (Show, Eq)

findGuardInitialStep :: [[Char]] -> Step
findGuardInitialStep m = ((guardXPosition, guardYPosition), guardDirection)
  where
    allRowsResult = map findGuardInRow m
    guardYPosition = case findIndex isJust allRowsResult of
      Just x -> x
      Nothing -> error "Couldn't find the guard's x position"
    (guardXPosition, guardDirection) = case allRowsResult !! guardYPosition of
      Just x -> x
      Nothing -> error "Couldn't find the guard's y position"

findGuardInRow :: [Char] -> Maybe (Int, Direction)
findGuardInRow row = case guardPosition of
  Just x -> Just (x, guardDirection x)
  Nothing -> Nothing
  where
    guardPosition = findIndex (\c -> c /= '.' && c /= '#') row
    guardDirection idx = case row !! idx of
      '^' -> Up
      'v' -> Down
      '<' -> Left
      '>' -> Right
      _ -> error "Unrecognised direction"

getTravelledRoute :: [[Char]] -> Position -> Direction -> [Step] -> Route
getTravelledRoute m currentPosition direction acc
  | nextPositionAndDirection `elem` acc = Loops acc
  | isOutOfBounds maxX maxY nextPosition = Exits ((currentPosition, direction) : acc)
  | otherwise = getTravelledRoute m nextPosition nextDirection ((currentPosition, direction) : acc)
  where
    nextPositionAndDirection =
      getNextPosition m maxX maxY currentPosition direction
    (nextPosition, nextDirection) =
      getNextPosition m maxX maxY currentPosition direction
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m

data Route = Exits [(Position, Direction)] | Loops [(Position, Direction)] deriving (Show)

loops :: Route -> Bool
loops r = case r of
  Exits _ -> False
  Loops _ -> True

getPositions :: Route -> [Position]
getPositions r = map fst $ case r of
  Exits route -> route
  Loops route -> route

isOutOfBounds :: Int -> Int -> Position -> Bool
isOutOfBounds maxX maxY (x, y) = x > maxX || y > maxY || x < 0 || y < 0

getNextPosition :: [[Char]] -> Int -> Int -> Position -> Direction -> (Position, Direction)
getNextPosition m maxX maxY currentPosition direction
  | isOutOfBounds maxX maxY naiveNextPosition = (naiveNextPosition, direction)
  | elementInNaiveNext == '#' = nextPositionRotated
  | otherwise = (naiveNextPosition, direction)
  where
    naiveNextPosition = moveInDirection direction currentPosition
    elementInNaiveNext = getElementInPosition m naiveNextPosition
    nextPositionRotated =
      getNextPosition
        m
        maxX
        maxY
        currentPosition
        (turnDirection direction)

moveInDirection :: Direction -> Position -> Position
moveInDirection direction (currentX, currentY) = case direction of
  Up -> (currentX, currentY - 1)
  Down -> (currentX, currentY + 1)
  Left -> (currentX - 1, currentY)
  Right -> (currentX + 1, currentY)

turnDirection :: Direction -> Direction
turnDirection direction = case direction of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Up

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

getElementInPosition :: [[a]] -> Position -> a
getElementInPosition m (x, y) = m !! y !! x

getDimensions :: [[a]] -> (Int, Int)
getDimensions m = (length $ head m, length m)
