{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Dec11 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M

day11file :: String
day11file = "src/day11input1.txt"

txtdata11 :: IO String
txtdata11 = readFile day11file

minput :: IO [[String]]
minput = fmap (fmap (splitOn ",") . lines) txtdata11

type Pos = (Integer, Integer)

toMove :: String -> Pos
toMove move
  | move == "n" = (0, 2)
  | move == "s" = (0, -2)
  | move == "ne" = (1, 1)
  | move == "nw" = (-1, 1)
  | move == "se" = (1, -1)
  | move == "sw" = (-1, -1)
  | otherwise    = (0, 0)

summoves :: [Pos] -> [Pos]
summoves = scanl' (\(a,b) (c,d) -> (a+c,b+d)) (0,0)

l1dist :: Pos -> Integer
l1dist (x0, y0) = (abs y0 + abs x0) `div` 2

day11answer1 = do
  input <- minput
  return $ fmap (last . fmap l1dist . summoves . fmap toMove) $ input

day11answer2 = do
  input <- minput
  return $ fmap (maximum . fmap l1dist . summoves . fmap toMove) $ input
