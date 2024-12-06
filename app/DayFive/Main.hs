{-# LANGUAGE LambdaCase #-}

module DayFive.Main (run) where

import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List (inits, sortBy)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Debug.Trace (trace)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

num :: Parsec String st Int
num = read <$> Parsec.many (Parsec.oneOf ['0' .. '9'])

parseDependencies = Parsec.many pair
 where
  pair = do
    l <- num
    _ <- Parsec.char '|'
    r <- num
    _ <- Parsec.newline
    return (l, r)
parsePageSequence = Parsec.sepBy1 num (Parsec.char ',') <* Parsec.newline
parseInput = do
  deps <- parseDependencies
  _ <- Parsec.spaces
  seqs <- Parsec.many parsePageSequence
  Parsec.eof
  return (dependencies deps, seqs)

dependencies :: [(Int, Int)] -> IntMap (Set Int)
dependencies = foldl go Map.empty
 where
  go :: IntMap (Set Int) -> (Int, Int) -> IntMap (Set Int)
  go m (k, v) = Map.insertWith Set.union k (Set.singleton v) m

order :: IntMap (Set Int) -> [Int] -> Int
order m xs =
  trace ("Init: " ++ show xs ++ "\nSorted: " ++ show sorted) $
    if xs == sorted
      then
        0
      else middle sorted
 where
  sorted = sortBy go xs
  hasEdge a b = case Map.lookup a m of
    Just ns -> Set.member b ns
    Nothing -> False
  go a b
    | hasEdge a b = LT
    | hasEdge b a = GT
    | otherwise = EQ
  middle n = n !! (length n `div` 2)
run :: IO ()
run = do
  input <- readFile "app/DayFive/input.txt"
  case Parsec.parse parseInput "Day5" input of
    Left e -> error $ show e
    Right (deps, seqs) ->
      print $ sum $ map (order deps) seqs
