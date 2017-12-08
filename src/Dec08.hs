{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Dec08 where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as M

day08file :: String
day08file = "src/day08input1.txt"

txtdata08 :: IO String
txtdata08 = readFile day08file

type Register = String
type Value = Int

type Modification = Value -> Value -> Value
type Comparer = Value -> Value -> Bool

type Bank = M.Map Register Value

data Command = Command Register Modification Value
data Condition = Condition Register Comparer Value
data Instruction = Instruction Command Condition

minput :: IO [Instruction]
minput = fmap (fmap parseLine . lines) txtdata08

parseLine :: String -> Instruction
parseLine = parseWords . words
  where
    parseWords (rn:mf:mv:_:cn:cmp:cv:_) = Instruction (Command rn pmf (read mv)) (Condition cn pcmp (read cv))
      where
        pmf
          | mf == "inc" = (+)
          | mf == "dec" = (-)
          | otherwise   = const
        pcmp
          | cmp == ">"  = (>)
          | cmp == ">=" = (>=)
          | cmp == "<"  = (<)
          | cmp == "<=" = (<=)
          | cmp == "==" = (==)
          | cmp == "!=" = (/=)
          | otherwise   = (\_ _ -> False)

runInstruction :: Instruction -> Bank -> Bank
runInstruction (Instruction (Command rn mf mv) (Condition cn cmp cv)) bank = update rn bank
  where
    access regname = M.findWithDefault 0 regname bank
    update regname = M.alter command regname
    command pval = case (access cn `cmp` cv) of
      False -> pval
      True -> Just $ access rn `mf` mv

runInstructions :: [Instruction] -> Bank
runInstructions = foldl' (flip runInstruction) M.empty

runInstructionsWithMem :: [Instruction] -> (Bank, Value)
runInstructionsWithMem = foldl' f (M.empty, 0)
  where
    f (bank, val) instr = (newbank, max val (maximum $ 0:(M.elems newbank)))
      where
        newbank = runInstruction instr bank

day08answer1 = do
  input <- minput
  let runs = runInstructions input
  let maxval = (head . sortOn negate . M.elems) runs
  return $ maxval

day08answer2 = do
  input <- minput
  let runs = runInstructionsWithMem input
  return $ snd runs
