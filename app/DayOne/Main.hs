{-# LANGUAGE TupleSections #-}

module DayOne.Main (run) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

locations :: [String] -> ([Int], [Int])
locations = unzip . go
 where
  go :: [String] -> [(Int, Int)]
  go (line : rest) =
    case words line of
      [left, right] -> (read left, read right) : go rest
      _ -> error "no numbers"
  go [] = []

run :: IO ()
run = do
  (left, right) <- locations . lines <$> readFile "app/DayOne/input.txt"
  print $ sum $ mult left $ getOccurs left right
 where
  occurences left = Map.fromList $ map (,0) left
  addOccurence m n = Map.adjust succ n m
  getOccurs left = foldl addOccurence (occurences left)
  mult left occurs = map (\n -> n * fromMaybe 1 (Map.lookup n occurs)) left
