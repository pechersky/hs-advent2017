module Dec13 where

import Data.List
import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseLines)

day13file :: String
day13file = "src/day13input1.txt"

type Depth = Int
type Range = Int

parseInput :: Parser (Depth, Range)
parseInput = (,) <$> decimal <* string ": " <*> decimal

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
  inputs <- parseLines day13file parseInput
  return $ cost 0 inputs

day13answer2 = do
  inputs <- parseLines day13file parseInput
  return $ seekcost inputs
