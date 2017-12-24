{-# LANGUAGE MultiWayIf #-}

module Dec24 where

import Data.List
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Tuple

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseLines)

day24file :: String
day24file = "src/day24input1.txt"

parseInput :: Parser (Int, Int)
parseInput = (,) <$> decimal <* "/" <*> decimal

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

-- build bridges by prepending each next link, so '0' has to be in the snd position
bridges :: StateT ([(Int, Int)], [(Int, Int)]) [] (Int, Int)
bridges = do
  (avail, used) <- get
  (p0, ps) <- lift $ select avail
  let p0' = case used of
              [] -> if snd p0 == 0 then p0 else swap p0
              (p:_) -> if match p0 p
                        then p0
                        else (swap p0)
  guard $ case used of
            [] -> snd p0' == 0
            (p:_) -> match p0' p
  put (ps, p0':used)
  return $ p0'
    where
      match :: (Int, Int) -> (Int, Int) -> Bool
      match (px, py) (x,y) = py == x

strength :: (Int, Int) -> Int
strength (x,y) = x + y

day24answer1 = do
  input <- parseLines day24file parseInput
  let posbridges = evalStateT (some bridges) (input, [])
  return $ maximum . fmap (sum . fmap strength) $ posbridges

day24answer2 = do
  input <- parseLines day24file parseInput
  let posbridges = evalStateT (some bridges) (input, [])
  return $ snd . maximum . fmap (\b -> (length b, sum $ fmap strength b)) $ posbridges
