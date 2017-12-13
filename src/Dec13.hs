{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Dec13 where

import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S

day13file :: String
day13file = "src/day13input1.txt"

txtdata13 :: IO String
txtdata13 = readFile day13file

minput :: IO [[String]]
minput = fmap (fmap (fmap (filter isDigit) . words) . lines) txtdata13

type Depth = Int
type Range = Int

parseInput :: [String] -> (Depth, Range)
parseInput (x:y:_) = (read x, read y)

cost :: Int -> [(Depth, Range)] -> Int
cost offset layerinfo =  sum . fmap (uncurry (*) . fst) . filter ((== 0) . snd) . zip layerinfo $ modpos
  where
    depths = fmap fst layerinfo
    ranges = fmap snd layerinfo
    cycles = fmap ((*2) . (subtract 1)) ranges
    modpos = zipWith mod (fmap (+ offset) depths) cycles

-- a seekcost that called cost to filter on
-- @cost offset layerinfo == 0@ took 2 minutes
seekcost :: [(Depth, Range)] -> Int
seekcost layerinfo = fst . head . filter (hit . snd) . zip times . fmap modpos $ times
  where
    depths = fmap fst layerinfo
    ranges = fmap snd layerinfo
    cycles = fmap ((*2) . (subtract 1)) ranges
    modpos offset = zipWith mod (fmap (+ offset) depths) cycles
    hit :: [Int] -> Bool
    hit = all (/= 0)
    times = [0..]

day13answer1 = do
  input <- minput
  return $ cost 0 $ fmap parseInput input

day13answer2 = do
  input <- minput
  return $ seekcost $ fmap parseInput input
