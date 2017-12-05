{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Dec05 where

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T

day05file :: String
day05file = "src/day05input1.txt"

txtdata05 :: IO String
txtdata05 = readFile day05file

instructions = fmap (toMap . fmap (head . intarr) . cell . rows . T.pack) txtdata05
  where
    rows :: T.Text -> [T.Text]
    rows = T.lines

    cell :: [T.Text] -> [[T.Text]]
    cell = fmap T.words

    intarr :: [T.Text] -> [Int]
    intarr = fmap (read . T.unpack)

    toMap :: [Int] -> M.Map Int Int
    toMap arr = M.fromList (zip [0..] arr)

followM offset = do
  startinst <- instructions
  let size = M.size startinst
  return (go size 0 0 startinst)
  where
    go !isize !ix !count !instr
      | ix >= isize = count
      | ix < 0 = count
      | otherwise = go isize newix (count + 1) $! newinstr
      where
        jump = instr M.! ix
        newix = ix + jump
        newinstr = M.update (Just . offset) ix instr

day05answer1 = followM (+1)
-- sometimes reaches a stack overflow, bang patterns might be fixing this
-- probably needs some more strictness somewhere or actual MVars
-- currently takes ~80 seconds for answer2
day05answer2 = followM offset
  where
    offset jump
      | jump >= 3 = (subtract 1) jump
      | otherwise = (+ 1) jump
