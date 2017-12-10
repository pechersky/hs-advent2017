{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Dec10 where

import Data.List
import Data.List.Split
import Data.Char
import Data.Bits
import Numeric (showHex)
import qualified Data.Text as T

day10file :: String
day10file = "src/day10input1.txt"

txtdata10 :: IO String
txtdata10 = readFile day10file

minput :: IO String
minput = fmap (head . lines) txtdata10

toInts :: String -> [Int]
toInts = fmap (read . T.unpack) . T.splitOn "," . T.pack

toCodes :: String -> [Int]
toCodes = (++ [17, 31, 73, 47, 23]) . fmap ord

toHex :: Int -> String
toHex val = reverse . take 2 . (++ "0") . reverse $ (showHex val "")

rehash :: [Int] -> Int -> Int -> Int -> (([Int], Int), Int)
rehash xs currPos skipVal moveLength = answer
  where
    listLength = length xs
    (begxs, endxs) = (splitAt currPos . cycle) xs
    (axs, bxs) = (splitAt moveLength) endxs
    revList = take listLength $ reverse axs ++ bxs
    currPos' = (currPos + moveLength + skipVal) `mod` listLength
    shiftPos = listLength - length begxs
    keepList = take listLength . drop shiftPos . cycle $ revList
    answer = ((keepList, currPos'), (succ skipVal))

hashfold = foldl' ((uncurry . uncurry) rehash) startval
  where
    startval = (([0..255], 0), 0)

day10answer1 = do
  input <- minput
  return $ product . take 2 . fst . fst . hashfold . toInts $ input

day10answer2 = do
  input <- minput
  return $ concat
         . fmap (toHex . foldl1 xor)
         . chunksOf 16
         . fst . fst
         . hashfold
         . concat . replicate 64
         . toCodes $ input
