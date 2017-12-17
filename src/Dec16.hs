module Dec16 where

import Data.List
import qualified Data.Map.Strict as M
import Control.Lens
import Data.List.Lens
import Data.Maybe

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseLines)

day16file :: String
day16file = "src/day16input1.txt"

type Program = Char
type Programs = [Program]
data Command = Spin Int | Swap Int Int | Partner Program Program
  deriving Show

parseCommand :: Parser Command
parseCommand = Spin    <$ char 's' <*> decimal
           <|> Swap    <$ char 'x' <*> decimal <* char '/' <*> decimal
           <|> Partner <$ char 'p' <*> anyChar <* char '/' <*> anyChar

runCommand :: Programs -> Command -> Programs
runCommand programs command = case command of
  Spin a -> let len = length programs in (take len . drop (len - a) . cycle) programs
  Swap a b -> let
    ca = programs !! a
    cb = programs !! b
    in (programs & ix a .~ cb) & ix b .~ ca
  Partner a b -> let
    xa = fromJust (elemIndex a programs)
    xb = fromJust (elemIndex b programs)
    in (programs & ix xa .~ b) & ix xb .~ a

inputprograms :: Programs
inputprograms = ['a'..'p']

memoizeByMap :: [Command] -> Programs -> M.Map Programs Programs -> (Programs, M.Map Programs Programs)
memoizeByMap commands prog oldmap = (ans, map)
  where
    (ans, map) = case (prog `M.lookup` oldmap) of
      Nothing -> let newprog = foldl' runCommand prog commands in (newprog, M.insert prog newprog oldmap)
      Just newprog -> (newprog, oldmap)

lookupMemoize :: [Command] -> Int -> Programs -> M.Map Programs Programs -> (Programs, M.Map Programs Programs)
lookupMemoize commands index prog map
  | M.size map == M.size map' = itlist !! (index `mod` M.size map')
  | otherwise = lookupMemoize commands (index - 1) newprog map'
    where
      itlist = iterate (uncurry (memoizeByMap commands)) $ (prog, map)
      (newprog, map') = head . tail $ itlist

day16answer1 = do
  commands <- head <$> parseLines day16file (parseCommand `sepBy1` char ',')
  return $ foldl' runCommand inputprograms commands

day16answer2 = do
  commands <- head <$> parseLines day16file (parseCommand `sepBy1` char ',')
  return $ fst $ lookupMemoize commands 1e9 inputprograms M.empty
