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
parseInput (x:y:xs) = (read x, read y)

layers :: [(Depth, Range)] -> Int -> [Int]
layers layerinfo time = fmap go layerinfo
  where
    go (depth, range) = scannerpos time
      where
        layer range = [0 .. range - 1] ++ (reverse [0 .. range - 2])
        scannerpos time = (layer range) !! (time `mod` (2*(range - 1)))

cost :: Int -> [(Depth, Range)] -> Int
cost offset layerinfo = sum scannerhit
  where
    maxdepth = maximum (fst <$> layerinfo)
    traveler = [offset .. maxdepth + offset]
    scannerpos = (layers layerinfo <$> traveler)
    scannerhit = fmap go (zip layerinfo [0 ..])
      where
        go ((depth, range), numlayer)
          | ((scannerpos !! depth) !! numlayer) == 0 = depth*range
          | otherwise = 0


seekcost layerinfo = fst . head . filter (hit . diag . snd) $ zip times $ fmap scannerpos times
  where
    times = [0 ..]
    maxdepth = maximum (fst <$> layerinfo)
    diag = zipWith ($) (fmap (flip (!!)) [0 ..])
    hit :: [Int] -> Bool
    hit = all (/= 0)
    scannerpos time = fmap go layerinfo
      where
        go (depth, range) = layers layerinfo (time + depth)



day13answer1 = do
  input <- minput
  return $ cost 0 $ fmap parseInput input

day13answer2 = do
  input <- minput
  return $ seekcost $ fmap parseInput input
