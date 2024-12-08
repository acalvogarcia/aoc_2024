{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.List (findIndex, group, groupBy, sort)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day08/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day08/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day08/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day08/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult getResultPart1
    getFileResultPart2 = getFileResult getResultPart2

getFileResult :: ([String] -> Int) -> String -> IO Int
getFileResult getResult file = do
  fileHandle <- readFile file
  let fileLines = lines fileHandle
  return $ getResult fileLines

getResultPart1 :: [String] -> Int
getResultPart1 m = length $ group $ sort allAntinodes
  where
    nodePositions = getNodePositions m
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m
    allAntinodes = concatMap (getAntinodePositionsPart1 maxX maxY . snd) nodePositions

getResultPart2 :: [String] -> Int
getResultPart2 m = length $ group $ sort allAntinodes
  where
    nodePositions = getNodePositions m
    (maxX, maxY) = bimap (subtract 1) (subtract 1) $ getDimensions m
    allAntinodes = concatMap (getAntinodePositionsPart2 maxX maxY . snd) nodePositions

getNodePositions :: [[Char]] -> [(Char, [Position])]
getNodePositions m = result
  where
    nodesPerRow = map (second getNodesInRow) $ enumerate m
    result = foldr (uncurry addRowNodesToMap) [] nodesPerRow
    addRowNodesToMap :: Int -> [(Char, [Int])] -> [(Char, [Position])] -> [(Char, [Position])]
    addRowNodesToMap idx cMaps mInner = foldr (addNodesToMap idx) mInner cMaps

addNodesToMap :: Int -> (Char, [Int]) -> [(Char, [Position])] -> [(Char, [Position])]
addNodesToMap rowIdx (c, nodes) m = result
  where
    existingIndex = findIndex (\(cInner, _) -> cInner == c) m
    existingPositions = case existingIndex of
      Just i -> snd $ m !! i
      Nothing -> []
    newPositions = existingPositions ++ map (,rowIdx) nodes
    result = case existingIndex of
      Just i -> swapElementInList m (c, newPositions) i
      Nothing -> (c, newPositions) : m

getNodesInRow :: [Char] -> [(Char, [Int])]
getNodesInRow r = result
  where
    nodePositions = filter (\(_, c) -> c /= '.') $ enumerate r
    groupedPositions = groupBy (\a b -> snd a == snd b) nodePositions
    result = map (\l -> (snd $ head l, map fst l)) groupedPositions

getAntinodePositionsPart1 :: Int -> Int -> [Position] -> [Position]
getAntinodePositionsPart1 maxX maxY nodePositions = map head $ group $ sort allValidAntinodes
  where
    allNodeCombinations = getAllCombinations nodePositions
    allAntinodePositions = concatMap (uncurry getPairAntinodePositionsPart1) allNodeCombinations
    allValidAntinodes = filter isValidPosition allAntinodePositions
    isValidPosition (x, y) = 0 <= x && x <= maxX && 0 <= y && y <= maxY

getAntinodePositionsPart2 :: Int -> Int -> [Position] -> [Position]
getAntinodePositionsPart2 maxX maxY nodePositions = map head $ group $ sort allValidAntinodes
  where
    allNodeCombinations = getAllCombinations nodePositions
    allAntinodePositions = concatMap (uncurry (getPairAntinodePositionsPart2 maxX maxY)) allNodeCombinations
    allValidAntinodes = filter isValidPosition allAntinodePositions
    isValidPosition (x, y) = 0 <= x && x <= maxX && 0 <= y && y <= maxY

data NodesRelation = PositiveSlope | Horizontal | Vertical | NegativeSlope deriving (Show)

getNodesRelation :: Position -> Position -> NodesRelation
getNodesRelation nA nB
  | nA > nB = getNodesRelation nB nA
  | fst nA == fst nB = Horizontal
  | snd nA == snd nB = Vertical
  | snd nA < snd nB = PositiveSlope
  | snd nA > snd nB = NegativeSlope
  | otherwise = error $ "How did we get here? " ++ errorMessage
  where
    errorMessage = show nA ++ show nB

getPairAntinodePositionsPart1 :: Position -> Position -> [Position]
getPairAntinodePositionsPart1 nodeA nodeB = outsideValues ++ insideValues
  where
    (x1, y1) = min nodeA nodeB
    (x2, y2) = max nodeA nodeB
    relation = getNodesRelation nodeA nodeB
    outsideValues = case relation of
      Horizontal -> [(x1 - dX, y1), (x2 + dX, y1)]
      Vertical -> [(x1, y1 - dY), (x2, y1 + dY)]
      NegativeSlope -> [(x1 - dX, y1 + dY), (x2 + dX, y2 - dY)]
      PositiveSlope -> [(x1 - dX, y1 - dY), (x2 + dX, y2 + dY)]
    hasInsideValues = dX `mod` 3 == 0 && dY `mod` 3 == 0
    possibleInsideValues = case relation of
      Horizontal -> [(x1 + xInnerDistance, y1), (x2 - xInnerDistance, y2)]
      Vertical -> [(x1, y1 + yInnerDistance), (x2, y2 - yInnerDistance)]
      NegativeSlope -> [(x1 + xInnerDistance, y1 - xInnerDistance), (x2 - xInnerDistance, y2 + yInnerDistance)]
      PositiveSlope -> [(x1 + xInnerDistance, y1 + xInnerDistance), (x2 - xInnerDistance, y2 - yInnerDistance)]
    insideValues = if hasInsideValues then possibleInsideValues else []
    dX = abs (x2 - x1)
    dY = abs (y2 - y1)
    xInnerDistance = dX `div` 3
    yInnerDistance = dY `div` 3

getPairAntinodePositionsPart2 :: Int -> Int -> Position -> Position -> [Position]
getPairAntinodePositionsPart2 maxX maxY nodeA nodeB = outsideElements
  where
    (x1, y1) = min nodeA nodeB
    (x2, y2) = max nodeA nodeB
    relation = getNodesRelation nodeA nodeB
    outsideElements = case relation of
      Horizontal -> getHorizontalAntinodePositions (x1, y1) (x2, y2) dX
      Vertical -> getVerticalAntinodePositions (x1, y1) (x2, y2) dY
      NegativeSlope -> getNegativeSlopeAntinodePositions (x1, y1) (x2, y2) dX dY
      PositiveSlope -> getPositiveSlopeAntinodePositions (x1, y1) (x2, y2) dX dY
    dX = abs (x2 - x1)
    dY = abs (y2 - y1)
    isValidPosition (x, y) = 0 <= x && x <= maxX && 0 <= y && y <= maxY
    getHorizontalAntinodePositions :: Position -> Position -> Int -> [Position]
    getHorizontalAntinodePositions (x1, y) (x2, _) dX = leftValues ++ rightValues
      where
        leftValues = truncateListComprehension isValidPosition [(x, y) | x <- [x1, x1 - dX ..]] []
        rightValues = truncateListComprehension isValidPosition [(x, y) | x <- [x2, x2 + dX ..]] []
    getVerticalAntinodePositions :: Position -> Position -> Int -> [Position]
    getVerticalAntinodePositions (x, y1) (_, y2) dY = leftValues ++ rightValues
      where
        leftValues = truncateListComprehension isValidPosition [(x, y) | y <- [y1, y1 - dY]] []
        rightValues = truncateListComprehension isValidPosition [(x, y) | y <- [y2, y2 + dY]] []
    getNegativeSlopeAntinodePositions :: Position -> Position -> Int -> Int -> [Position]
    getNegativeSlopeAntinodePositions (x1, y1) (x2, y2) dX dY = leftValues ++ rightValues
      where
        leftXGenerator = [x1, x1 - dX ..]
        leftYGenerator = [y1, y1 + dY ..]
        rightXGenerator = [x2, x2 + dX ..]
        rightYGenerator = [y2, y2 - dY ..]
        leftValues = truncateListComprehension isValidPosition (zip leftXGenerator leftYGenerator) []
        rightValues = truncateListComprehension isValidPosition (zip rightXGenerator rightYGenerator) []
    getPositiveSlopeAntinodePositions :: Position -> Position -> Int -> Int -> [Position]
    getPositiveSlopeAntinodePositions (x1, y1) (x2, y2) dX dY = leftValues ++ rightValues
      where
        leftXGenerator = [x1, x1 - dX ..]
        leftYGenerator = [y1, y1 - dY ..]
        rightXGenerator = [x2, x2 + dX ..]
        rightYGenerator = [y2, y2 + dY ..]
        leftValues = truncateListComprehension isValidPosition (zip leftXGenerator leftYGenerator) []
        rightValues = truncateListComprehension isValidPosition (zip rightXGenerator rightYGenerator) []

type Position = (Int, Int)

getAllCombinations :: [Position] -> [(Position, Position)]
getAllCombinations [] = []
getAllCombinations (p : ps) = map (p,) ps ++ getAllCombinations ps

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

swapElementInList :: [a] -> a -> Int -> [a]
swapElementInList originalList element position = elementsBefore ++ element : elementsAfter
  where
    -- let nodePositions = getNodePositions fileLines
    -- print $ map (second getNodesInRow) $ enumerate fileLines
    -- print $ getNodePositions fileLines
    -- return 5

    (elementsBefore, elementsFrom) = splitAt position originalList
    elementsAfter = tail elementsFrom

getDimensions :: [[a]] -> (Int, Int)
getDimensions m = (length $ head m, length m)

truncateListComprehension :: (a -> Bool) -> [a] -> [a] -> [a]
truncateListComprehension _ [] acc = acc
truncateListComprehension f (x : xs) acc
  | f x = truncateListComprehension f xs (acc ++ [x])
  | otherwise = acc
