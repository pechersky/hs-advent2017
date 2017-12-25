{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Dec25 where

import Data.List
import qualified Data.List.Split as LS
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as S

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseInput)

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State.Strict as ST
import Control.Monad.Trans.Class
import Data.Char
import Data.Maybe
import Data.Tuple

day25file :: String
day25file = "src/day25input1.txt"

parseFile :: String -> (Char, Int, MInstr)
parseFile (lines->b:p:ls) = ((head $ last $ words b) ,
                               (read $ head $ reverse $ init $ words p) ,
                               foldr M.union M.empty (go ls))
  where
    go [] = []
    go (_:s:l0:v0:m0:s0:l1:v1:m1:s1:ls) = M.singleton (head $ last $ words s) (M.fromList [
                                            ((read $ f $ last $ words l0), (
                                            (read $ f $ last $ words v0),
                                            (f $ last $ words m0),
                                            (head $ last $ words s0))) ,
                                           ((read $ f $ last $ words l1), (
                                            (read $ f $ last $ words v1),
                                            (f $ last $ words m1),
                                            (head $ last $ words s1)))]) : go ls
    go _ = []
    f = filter isAlphaNum


type SInstr = (Char, Instr, Instr)
type Instr = (Int, Int, String, Char)
type MInstr = M.Map Char (M.Map Int (Int, String, Char))

runSet :: MInstr -> (Char, Int, IM.IntMap Int) -> (Char, Int, IM.IntMap Int)
runSet instructions (!state, !cursor, !tape) = (state', cursor', tape')
  where
    instr = instructions M.! state
    curr = IM.findWithDefault 0 cursor tape
    (next, movestr, state') = instr M.! curr
    tape' = case next of
              0 -> IM.delete cursor tape
              _ -> IM.insert cursor next tape
    cursor' = case movestr of
                "left" -> cursor - 1
                "right" -> cursor + 1
                _ -> error "not a move"

day25answer1 = do
  input <- readFile day25file
  (startState, numSteps, instrs) <- return $ parseFile input
  {-return $ sum $ evalState (replicateM numSteps (runTape instrs)) (startState, tapeOf 0)-}
  return $ sum . IM.elems . (\(_,_,x) -> x) . (!! numSteps) $ iterate (runSet instrs) (startState, 0, IM.empty)
