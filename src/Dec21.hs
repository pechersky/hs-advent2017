module Dec21 where

import Data.List
import qualified Data.List.Split as LS
import qualified Data.Map.Strict as M

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseLines)

day21file :: String
day21file = "src/day21input1.txt"

type Grid = [String]

parseInput :: Parser (Grid, Grid)
parseInput = (,) <$> valid <* string " => " <*> valid
  where
    valid :: Parser Grid
    valid = some (oneOf (".#" :: String)) `sepBy1` "/"

toMap :: [(Grid, Grid)] -> M.Map Grid Grid
toMap = M.fromList . concat . fmap go
  where
    go (x, y) = [(x', y) | x' <- allRule x]

startGrid = [".#.","..#","###"]

breakBy2 :: M.Map Grid Grid -> Grid -> Grid
breakBy2 map = concat . fmap (
                fmap concat . transpose
              . fmap (map M.!)
              . transpose . fmap (LS.chunksOf 2)
              ) . LS.chunksOf 2

breakBy3 :: M.Map Grid Grid -> Grid -> Grid
breakBy3 map = concat . fmap (
                fmap concat . transpose
              . fmap (map M.!)
              . transpose . fmap (LS.chunksOf 3)
              ) . LS.chunksOf 3

breakGrid :: M.Map Grid Grid -> Grid -> Grid
breakGrid m grid
  | length grid `mod` 2 == 0 = breakBy2 m grid
  | otherwise = breakBy3 m grid

flipRule :: Grid -> Grid
flipRule = fmap reverse

rotateRule :: Grid -> Grid
rotateRule = flipRule . transpose

allRule :: Grid -> [Grid]
allRule rule = do
  f <- [id, flipRule]
  g <- [id, rotateRule, rotateRule . rotateRule, rotateRule . rotateRule . rotateRule]
  return $ (g . f) rule

pixelsOn :: Grid -> Int
pixelsOn = length . filter (== '#') . concat

day21answer1 = do
  input <- parseLines day21file parseInput
  let map = toMap input
  return $ pixelsOn . (!! 5) . iterate (breakGrid map) $ startGrid

day21answer2 = do
  input <- parseLines day21file parseInput
  let map = toMap input
  return $ pixelsOn . (!! 18) . iterate (breakGrid map) $ startGrid
