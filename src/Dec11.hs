{-# LANGUAGE ViewPatterns #-}

module Dec11 where

import Data.List
import Data.List.Split
import Linear.Affine
import Linear.V2

day11file :: String
day11file = "src/day11input1.txt"

txtdata11 :: IO String
txtdata11 = readFile day11file

minput :: IO [[String]]
minput = fmap (fmap (splitOn ",") . lines) txtdata11

type Pos = Point V2 Integer
type Step = Diff V2 Integer

toMove :: String -> Step
toMove move
  | move == "n"  = V2 ( 0) ( 2)
  | move == "s"  = V2 ( 0) (-2)
  | move == "ne" = V2 ( 1) ( 1)
  | move == "se" = V2 ( 1) (-1)
  | move == "nw" = V2 (-1) ( 1)
  | move == "sw" = V2 (-1) (-1)
  | otherwise    = V2 ( 0) ( 0)

summoves :: [Step] -> [Pos]
summoves = scanl' (.+^) origin

-- need to consider case of "ne,se"
l1dist :: Pos -> Integer
l1dist point@(unP->V2 x y)
  | ax < ay   = sum (fmap abs point) `div` 2
  | otherwise = sum (fmap abs point)
  where
    ax = abs x
    ay = abs y

day11answer1 = do
  input <- minput
  return $ last . fmap (last . fmap l1dist . summoves . fmap toMove) $ input

day11answer2 = do
  input <- minput
  return $ last . fmap (maximum . fmap l1dist . summoves . fmap toMove) $ input
