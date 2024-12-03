module Split (splitOnce, chunks) where

import Data.List (stripPrefix)

splitOnce :: (Eq a) => [a] -> [a] -> [a] -> ([a], [a])
splitOnce comp acc list =
  case stripPrefix comp list of
    Just rest -> (acc, rest)
    Nothing -> splitOnce comp (acc ++ take 1 list) (tail list)

chunks :: Int -> [a] -> [a] -> [[a]]
chunks _ acc []
  | null acc = []
  | otherwise = [acc]
chunks size acc (c : cs)
  | length acc == size = acc : chunks size [c] cs
  | otherwise = chunks size (acc ++ [c]) cs
