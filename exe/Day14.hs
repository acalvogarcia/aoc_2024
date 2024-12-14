import Data.Bifunctor (bimap)
import Data.List (intercalate)
import GHC.Base (when)
import Split (splitOnce)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResult "input/day14/test-input.txt" 11 7
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResult "input/day14/input.txt" 101 103
  printf "Part 1 test result: %d\n" part_1_result
  getPart2Picture "input/day14/input.txt" 101 103 0

getFileResult :: String -> Int -> Int -> IO Int
getFileResult file mapLength mapHeight = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let allFinalPositions = map (getPositionAtT mapLength mapHeight 100 . getRobotParams) fileLines
  let allQuadrants = getPositionsByQuadrant mapLength mapHeight allFinalPositions
  return $ foldr ((*) . length) 1 allQuadrants

getPart2Picture :: String -> Int -> Int -> Int -> IO ()
getPart2Picture file mapLength mapHeight t = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  let allFinalPositions = map (getPositionAtT mapLength mapHeight t . getRobotParams) fileLines
  let charsMap = getCharsMap mapLength mapHeight allFinalPositions
  let transposedCharsMap = transpose charsMap
  let showChars = has8InARow charsMap
  let showTransposedChars = has8InARow transposedCharsMap
  when showChars $
    do
      printPicture charsMap
      print $ "Time: " ++ show t
  when showTransposedChars $
    do
      printPicture transposedCharsMap
      print $ "Time: " ++ show t
  when (showChars || showTransposedChars) $
    do
      _ <- getLine
      putStr ""
  getPart2Picture file mapLength mapHeight (t + 1)

printPicture :: [[Char]] -> IO ()
printPicture chars = do
  let picture = intercalate "\n" chars
  putStr picture
  print ""

has8InARow :: [[Char]] -> Bool
has8InARow = any lineHas8InARow

lineHas8InARow :: [Char] -> Bool
lineHas8InARow chars
  | length chars < n = False
  | otherwise = thisStepTrue || lineHas8InARow (tail chars)
  where
    n = 8
    thisStepTrue = replicate n 'X' == take n chars

getCharsMap :: Int -> Int -> [Position] -> [[Char]]
getCharsMap mapLength mapHeight positions = map getLineChars [0 .. mapHeight - 1]
  where
    getLineChars y = map (\x -> if Position {x, y} `elem` positions then 'X' else '.') [0 .. mapLength - 1]

getPositionsByQuadrant :: Int -> Int -> [Position] -> [[Position]]
getPositionsByQuadrant mapLength mapHeight positions = [upLeft, upRight, downLeft, downRight]
  where
    leftMax = (mapLength `div` 2) - 1
    rightMin = leftMax + 2
    upMax = (mapHeight `div` 2) - 1
    downMin = upMax + 2
    upLeft = filter (\Position {x, y} -> x <= leftMax && y <= upMax) positions
    upRight = filter (\Position {x, y} -> x >= rightMin && y <= upMax) positions
    downLeft = filter (\Position {x, y} -> x <= leftMax && y >= downMin) positions
    downRight = filter (\Position {x, y} -> x >= rightMin && y >= downMin) positions

getPositionAtT :: Int -> Int -> Int -> RobotParameters -> Position
getPositionAtT mapLength mapHeight t RobotParameters {x0 = x0, y0 = y0, vX = vX, vY = vY} =
  Position {x = x, y = y}
  where
    naiveX = x0 + t * vX
    naiveY = y0 + t * vY
    xRem = naiveX `rem` mapLength
    yRem = naiveY `rem` mapHeight
    x = if xRem >= 0 then xRem else mapLength + xRem
    y = if yRem >= 0 then yRem else mapHeight + yRem

getRobotParams :: String -> RobotParameters
getRobotParams input = RobotParameters {x0 = x, y0 = y, vX = vX, vY = vY}
  where
    (posString, velString) = splitOnce " v=" [] $ drop 2 input
    (x, y) = getParams posString
    (vX, vY) = getParams velString
    getParams :: String -> (Int, Int)
    getParams s = bimap read read $ splitOnce "," [] s

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

data RobotParameters = RobotParameters
  { x0 :: Int,
    y0 :: Int,
    vX :: Int,
    vY :: Int
  }
  deriving (Show)

transpose :: [[a]] -> [[a]]
transpose m = map (\x -> map (getElementInPosition m x) [0 .. maxY]) [0 .. maxX]
  where
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m

getDimensions :: [[a]] -> (Int, Int)
getDimensions m = (length $ head m, length m)

getElementInPosition :: [[a]] -> Int -> Int -> a
getElementInPosition m x y = m !! y !! x
