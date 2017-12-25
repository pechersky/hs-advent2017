{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Dec25 where

import Data.List
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Lazy as IM
import Control.Applicative

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseInput)


day25file :: String
day25file = "src/day25input1.txt"

type PState = Char
parsePState = "state " *> anyChar

type Cursor = Int

type Value = Int
parseValue = decimal

data Move = MLeft | MRight
  deriving Show
parseMove = choice [MLeft <$ "left", MRight <$ "right"]

skipTo :: Parser a -> Parser a
skipTo = skipSomeTill (notChar '\n')

type MInstr = M.Map PState (M.Map Value (Value, Move, PState))
parseMInstr = M.singleton <$  newline
                          <*  "In " <*> parsePState <* newline
                          <*> (mconcat <$> some parseStep)
  where
    parseStep :: Parser (M.Map Value (Value, Move, PState))
    parseStep =  M.singleton <$> skipTo parseValue <* newline
                             <*> parseBlock
                             <*  optional newline
    parseBlock :: Parser (Value, Move, PState)
    parseBlock = (,,) <$> skipTo parseValue  <* newline
                      <*> skipTo parseMove   <* newline
                      <*> skipTo parsePState

parseInstr :: Parser (PState, Int, MInstr)
parseInstr = (,,) <$ "Begin in " <*> parsePState  <* newline
                  <*> skipTo decimal <* skipTo newline
                  <*> (mconcat <$> some parseMInstr)
                  <*  eof

runSet :: MInstr -> (PState, Cursor, IM.IntMap Value) -> (PState, Cursor, IM.IntMap Value)
runSet instructions (!state, !cursor, !tape) = (state', cursor', tape')
  where
    instr = instructions M.! state
    curr = IM.findWithDefault 0 cursor tape
    (next, move, state') = instr M.! curr
    tape' = case next of
              0 -> IM.delete cursor tape
              _ -> IM.insert cursor next tape
    cursor' = case move of
                MLeft  -> cursor - 1
                MRight -> cursor + 1

day25answer1 = do
  input <- readFile day25file
  (startState, numSteps, instrs) <- parseInput parseInstr $ filter (not . isPunctuation) input
  return $ sum . IM.elems . (\(_,_,x) -> x) . (!! numSteps) $ iterate (runSet instrs) (startState, 0, IM.empty)


data Tape a = Tape {_lefts :: [a], _curr :: a, _rights :: [a], _bounds :: !(Int, Int)}
  deriving (Functor)

instance (Show a) => Show (Tape a)
  where
    show Tape{..} = show (reverse (take 3 _lefts)) ++ " " ++ show _curr ++ " " ++ show (take 3 _rights) ++ " " ++ show _bounds

tapeOf :: a -> Tape a
tapeOf a = Tape (repeat a) a (repeat a) (0,0)

moveL, moveR :: Tape a -> Tape a
moveL (Tape (l:ls) x rs (!bl, !br)) = Tape ls l (x:rs) (max 0 (bl-1), max 0 (br+1))
moveR (Tape ls x (r:rs) (!bl, !br)) = Tape (x:ls) r rs (max 0 (bl+1), max 0 (br-1))

setTape :: Tape a -> a -> Tape a
setTape Tape{..} a = Tape _lefts a _rights _bounds

runTape :: MInstr -> (PState, Tape Value) -> (PState, Tape Value)
runTape instructions (!state, !tape) = (state', tape')
  where
    instr = instructions M.! state
    curr = _curr tape
    (next, move, state') = instr M.! curr
    tape' = ($ setTape tape next) $ case move of
                                      MLeft  -> moveL
                                      MRight -> moveR

sumTape :: Num a => Tape a -> a
sumTape Tape{..} = sum (take (fst _bounds) _lefts) + _curr + sum (take (snd _bounds) _rights)

day25answer1b = do
  input <- readFile day25file
  (startState, numSteps, instrs) <- parseInput parseInstr $ filter (not . isPunctuation) input
  return $ sumTape . snd . (!! numSteps) $ iterate (runTape instrs) (startState, tapeOf 0)
