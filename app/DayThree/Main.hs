module DayThree.Main (run) where

import Data.Functor (($>))
import GHC.Base (liftA)
import GHC.IO (unsafePerformIO)
import Text.Parsec

num :: Parsec String st Int
num = read <$> many (oneOf ['0' .. '9'])

pair :: Parsec String st (Int, Int)
pair = do
  x <- num
  _ <- char ','
  y <- num
  return (x, y)

partOne :: String -> Either ParseError Int
partOne = parse parseInput "Day3"
 where
  mul = uncurry (*) <$> (string "mul" *> between (char '(') (char ')') pair)
  parseMuls = manyTill anyChar (try $ lookAhead mul) >> mul
  parseInput = sum <$> many (try parseMuls) <* manyTill anyChar eof <* eof

data Instruction
  = Do
  | Don't
  | Mul Int Int
  deriving (Show)

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions = parse allInstructions "partTwo"
 where
  do' = string "do()" $> Do
  dont = string "don't()" $> Don't
  mul = uncurry Mul <$> (string "mul" >> between (char '(') (char ')') pair)
  instruction = try do' <|> try dont <|> try mul
  oneInstruction = do
    _ <- manyTill anyChar (try $ lookAhead instruction)
    instruction
  allInstructions = do
    inst <- many (try oneInstruction)
    _ <- manyTill anyChar eof
    return inst

partTwo :: [Instruction] -> Int
partTwo = go True 0
 where
  go :: Bool -> Int -> [Instruction] -> Int
  go active acc (Mul x y : ops) =
    if active
      then go active (x * y + acc) ops
      else go active acc ops
  go False acc (Do : ops) = go True acc ops
  go True acc (Don't : ops) = go False acc ops
  go active acc (_ : ops) = go active acc ops
  go _ acc [] = acc

run :: IO ()
run = do
  input <- parseInstructions <$> readFile "app/DayThree/input.txt"
  case input of
    Left e -> print e
    Right instructions -> do
      print instructions
      print $ partTwo instructions
