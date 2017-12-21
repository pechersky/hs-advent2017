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

parseInput :: Parser (String, String)
parseInput = (,) <$> valid <* string " => " <*> valid
  where
    valid :: Parser String
    valid = many (oneOf (".#/" :: String))

startGrid = [".#.","..#","###"]

breakBy2 :: M.Map [String] [String] -> [String] -> [String]
breakBy2 map = concat . fmap (
                fmap concat . transpose
              . fmap (map M.!)
              . transpose . fmap (LS.chunksOf 2)
              ) . LS.chunksOf 2

breakBy3 :: M.Map [String] [String] -> [String] -> [String]
breakBy3 map = concat . fmap (
                fmap concat . transpose
              . fmap (map M.!)
              . transpose . fmap (LS.chunksOf 3)
              ) . LS.chunksOf 3

breakGrid :: M.Map [String] [String] -> [String] -> [String]
breakGrid m grid
  | length grid `mod` 2 == 0 = breakBy2 m grid
  | otherwise = breakBy3 m grid

toMap :: [(String, String)] -> M.Map [String] [String]
toMap = M.fromList . concat . fmap go
  where
    go (go'->x, go'->y) = [(x', y) | x' <- allRule x]
    go' = LS.splitOn "/"

flipRule :: [String] -> [String]
flipRule = fmap reverse

rotateRule :: [String] -> [String]
rotateRule = flipRule . transpose

allRule :: [String] -> [[String]]
allRule rule = do
  f <- [id, flipRule]
  g <- [id, rotateRule, rotateRule . rotateRule, rotateRule . rotateRule . rotateRule]
  return $ (g . f) rule

pixelsOn :: [String] -> Int
pixelsOn = length . filter (== '#') . concat

day21answer1 = do
  input <- parseLines day21file parseInput
  let map = toMap input
  return $ pixelsOn . (!! 5) . iterate (breakGrid map) $ startGrid

day21answer2 = do
  input <- parseLines day21file parseInput
  let map = toMap input
  return $ pixelsOn . (!! 18) . iterate (breakGrid map) $ startGrid
