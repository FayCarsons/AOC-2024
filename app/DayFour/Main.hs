module DayFour.Main (run) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

type Mat = Vector (Vector Char)

readToVec :: IO Mat
readToVec = do
  input <- readFile "app/DayFour/input.txt"
  return $ Vec.fromList $ map Vec.fromList (lines input)

offsets :: Int -> Int -> [[(Int, Int)]]
offsets x y = [[(pred x, succ y), (succ x, pred y)], [(pred x, pred y), (succ x, succ y)]]

readXmas :: Mat -> Int
readXmas mat = foldl go 0 coords
 where
  boundX = length mat
  boundY = length (mat ! 0)
  coords = [(x, y) | x <- [0 .. pred boundX], y <- [0 .. pred boundY]]
  vlookup x y
    | x >= 0 && y >= 0 && x < boundX && y < boundY = Just $ (mat ! x) ! y
    | otherwise = Nothing
  isXmas coords' =
    case mapM (uncurry vlookup) coords' of
      Just "MS" -> True
      Just "SM" -> True
      _ -> False
  countXmas x y =
    if vlookup x y == Just 'A' && all isXmas (offsets x y)
      then 1
      else 0
  go acc (x, y) = acc + countXmas x y

run :: IO ()
run = do
  mat <- readToVec
  print $ readXmas mat
