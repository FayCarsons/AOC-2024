module DayTwo.Main (run) where

import Data.List (inits, sort, tails)
import GHC.Base (assert)

absDiff :: Int -> Int -> Int
absDiff a b = abs (a - b)

valid :: [Int] -> Bool
valid report = isMonotonic report && isStable report
 where
  differences :: [Int] -> [Int]
  differences xs = zipWith absDiff xs $ tail xs
  isMonotonic xs = xs == sort xs || report == reverse (sort xs)
  isStable xs = and [ds `elem` [1 .. 3] | ds <- differences xs]

validWithError :: [Int] -> Bool
validWithError report = valid `any` combinations
 where
  combinations = zipWith (++) (inits report) (tail $ tails report)

numValid :: String -> ([Int] -> Bool) -> Int
numValid inp f = length $ filter f $ map (map read . words) $ lines inp

run :: IO ()
run = do
  input <- readFile "app/DayTwo/input.txt"
  _ <- return $ assert (402 == numValid input valid)
  _ <- return $ assert (455 == numValid input validWithError)
  return ()
