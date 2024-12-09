import Data.Char (ord)
import Data.List (findIndex)
import Data.Maybe (isNothing)
import Split (chunks)
import Text.Printf (printf)

main :: IO ()
main = do
  part_1_test_result <- getFileResultPart1 "input/day09/test-input.txt"
  printf "Part 1 test result: %d\n" part_1_test_result
  part_1_result <- getFileResultPart1 "input/day09/input.txt"
  printf "Part 1 result: %d\n" part_1_result
  part_2_test_result <- getFileResultPart2 "input/day09/test-input.txt"
  printf "Part 2 test result: %d\n" part_2_test_result
  part_2_result <- getFileResultPart2 "input/day09/input.txt"
  printf "Part 2 result: %d\n" part_2_result
  where
    getFileResultPart1 = getFileResult True
    getFileResultPart2 = getFileResult False

getFileResult :: Bool -> String -> IO Int
getFileResult part1 file = do
  fileHandle <- readFile file
  let input = map (\c -> ord c - 48) $ init fileHandle
  return $ getResult part1 input

getResult :: Bool -> [Int] -> Int
getResult part1 input = getResultFromRearrangedData rearrangedData 0
  where
    extractedData = extractData input
    rearrangedData =
      if part1
        then rearrangeDataPart1 extractedData
        else rearrangeDataPart2 extractedData (length extractedData - 1)

getResultFromRearrangedData :: [MemoryData] -> Int -> Int
getResultFromRearrangedData [] _ = 0
getResultFromRearrangedData ((fileID, len, space) : ds) startIdx =
  thisStepResult + getResultFromRearrangedData ds (startIdx + len + space)
  where
    thisStepResult = sum $ map (fileID *) $ take len [startIdx ..]

rearrangeDataPart1 :: [(Int, Int, Int)] -> [(Int, Int, Int)]
rearrangeDataPart1 input
  | isNothing firstElementWithSpace = input
  | firstSpaceIdx == (length input - 1) = input
  | otherwise = rearrangeDataPart1 newInput
  where
    firstElementWithSpace = findIndex (\(_, _, s) -> s /= 0) input
    firstSpaceIdx = case firstElementWithSpace of
      Just i -> i
      Nothing -> error "Found no element with space"
    elementsBeforeFirstSpace = take firstSpaceIdx input
    allElementsAfterFirstSpace = drop (firstSpaceIdx + 1) input
    elementsAfterFirstSpace
      | null allElementsAfterFirstSpace = []
      | otherwise = init allElementsAfterFirstSpace
    (withSpaceId, withSpaceLen, space) = input !! firstSpaceIdx
    (lastElementId, lastElementLen, _) = last input
    ((insElemA, insElemB), newLastElement) :: ((MemoryData, MemoryData), Maybe MemoryData)
      | lastElementLen > space = (((withSpaceId, withSpaceLen, 0), (lastElementId, space, 0)), Just (lastElementId, lastElementLen - space, 0))
      | otherwise = (((withSpaceId, withSpaceLen, 0), (lastElementId, lastElementLen, space - lastElementLen)), Nothing)
    newInput =
      elementsBeforeFirstSpace ++ [insElemA, insElemB] ++ elementsAfterFirstSpace ++ case newLastElement of
        Just e -> [e]
        Nothing -> []

rearrangeDataPart2 :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
rearrangeDataPart2 input 0 = input
rearrangeDataPart2 input idToCheck
  | isNothing firstElementWithSpace = rearrangeDataPart2 input nextIdToCheck
  | firstSpaceIdx >= elementToMoveIdx = rearrangeDataPart2 input nextIdToCheck
  | otherwise = rearrangeDataPart2 newInput nextIdToCheck
  where
    elementToMoveIdx = case findIndex (\(fileId, _, _) -> fileId == idToCheck) input of
      Just i -> i
      Nothing -> error $ "Couldn't find element by index " ++ show idToCheck ++ " - " ++ show input
    (_, elemLen, _) = input !! elementToMoveIdx
    firstElementWithSpace = findIndex (\(_, _, s) -> s >= elemLen) input
    firstSpaceIdx = case firstElementWithSpace of
      Just i -> i
      Nothing -> length input - 1
    spaceInFront = firstSpaceIdx == elementToMoveIdx - 1
    newInput =
      if spaceInFront
        then rearrangeOneElementSpaceInFront input elementToMoveIdx
        else rearrangeOneElement input elementToMoveIdx firstSpaceIdx
    nextIdToCheck = idToCheck - 1

rearrangeOneElementSpaceInFront :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
rearrangeOneElementSpaceInFront input position =
  swapElementInList (swapElementInList input newElement position) newInFront (position - 1)
  where
    (idInFront, lenInFront, spaceInFront) = input !! (position - 1)
    (idElement, lenElement, spaceElement) = input !! position
    newInFront = (idInFront, lenInFront, 0)
    newElement = (idElement, lenElement, spaceElement + spaceInFront)

rearrangeOneElement :: [(Int, Int, Int)] -> Int -> Int -> [(Int, Int, Int)]
rearrangeOneElement input oldPosition positionWithSpace = elementsBefore ++ newElement : elementsAfter
  where
    (idWithSpace, lenWithSpace, space) = input !! positionWithSpace
    (idElement, lenElement, spaceElement) = input !! oldPosition
    (idPrevious, lenPrevious, spacePrevious) = input !! (oldPosition - 1)
    newWithSpace = (idWithSpace, lenWithSpace, 0)
    newElement = (idElement, lenElement, space - lenElement)
    newPrevious = (idPrevious, lenPrevious, spacePrevious + spaceElement + lenElement)
    swappedPrevious = swapElementInList input newPrevious (oldPosition - 1)
    swappedWithSpace = swapElementInList swappedPrevious newWithSpace positionWithSpace
    withoutElement = take oldPosition swappedWithSpace ++ drop (oldPosition + 1) swappedWithSpace
    (elementsBefore, elementsAfter) =
      splitAt (positionWithSpace + 1) withoutElement

type MemoryData = (Int, Int, Int)

extractData :: [Int] -> [MemoryData]
extractData input = pairsWithIdx
  where
    rawPairs = chunks 2 [] input
    dataPairs = map (\p -> if length p == 2 then (head p, last p) else (head p, 0)) rawPairs
    pairsWithIdx = map (\(i, (a, b)) -> (i, a, b)) $ enumerate dataPairs

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

swapElementInList :: [a] -> a -> Int -> [a]
swapElementInList originalList element position = elementsBefore ++ element : elementsAfter
  where
    (elementsBefore, elementsFrom) = splitAt position originalList
    elementsAfter = tail elementsFrom
