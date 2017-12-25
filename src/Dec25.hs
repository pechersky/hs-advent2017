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
import Text.Megaparsec.Perm
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

parseInstr :: Parser (Char, Int, MInstr)
parseInstr = (,,) <$ "Begin in state " <*> anyChar
                  <*> skipSomeTill anyChar decimal
                  <*  skipSomeTill anyChar newline
                  <*> (foldr M.union M.empty <$> some (parseMI <* "." <* (optional newline)))
                  <*  eof
  where
    parseMI :: Parser MInstr
    parseMI = M.singleton <$ newline <* "In state " <*> anyChar
                          <*> (M.union <$> parseMove <*> parseMove)
    parseMove :: Parser (M.Map Int (Int, String, Char))
    parseMove =  M.singleton <$> skipSomeTill anyChar decimal
                             <*> parseBlock
    parseBlock :: Parser (Int, String, Char)
    parseBlock = (,,) <$> skipSomeTill anyChar decimal
                      <*> skipSomeTill anyChar (choice ["left", "right"])
                      <*  skipSomeTill anyChar "state " <*> anyChar


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
  (startState, numSteps, instrs) <- parseInput parseInstr input
  return $ sum . IM.elems . (\(_,_,x) -> x) . (!! numSteps) $ iterate (runSet instrs) (startState, 0, IM.empty)
