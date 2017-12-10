{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Dec08 where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict
import Control.Monad

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

-- run the instruction in a State that keeps the maximum Value
-- this could be faster for huge Instruction lists, in that we now
-- only check registers that we have modified, instead of all registers
runInstructionM :: Instruction -> Bank -> State Value Bank
runInstructionM (Instruction (Command rn mf mv) (Condition cn cmp cv)) bank = case (access cn `cmp` cv) of
  False -> return bank
  True  -> do
    modify' (max updatedVal)
    return $ M.insert rn updatedVal bank
  where
    access regname = M.findWithDefault 0 regname bank
    updatedVal = access rn `mf` mv

runInstructionsM :: [Instruction] -> State Value Bank
runInstructionsM = foldM (flip runInstructionM) M.empty

day08answer1 = do
  input <- minput
  let runs = runInstructionsM input
  return $ maximum $ evalState runs 0

day08answer2 = do
  input <- minput
  let runs = runInstructionsM input
  return $ execState runs 0
