{-# LANGUAGE TemplateHaskell #-}

module Dec20 where

import Data.List
import Data.Function

import Control.Lens

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseLines)

import Linear.V3

day20file :: String
day20file = "src/day20input1.txt"

type Vec = V3 Int
data Point = Point {_pos :: Vec, _vel :: Vec, _accel :: Vec}
  deriving Show

instance Eq Point where
  (==) = (==) `on` _pos

instance Ord Point where
  compare = compare `on` _pos

makeClassy ''Point

parseInput :: Parser Point
parseInput = Point <$ string "p=" <*> parseVec <* string ", v=" <*> parseVec <* string ", a=" <*> parseVec
  where
    parseVec :: Parser Vec
    parseVec = V3 <$ "<" <*> int <* "," <*> int <* "," <*> int <* ">"
    int = (L.signed space (L.decimal))

updatePoints :: [Point] -> [Point]
updatePoints = fmap update
  where
    update :: Point -> Point
    update prop = intprop & pos +~ (_vel intprop)
      where
        intprop = prop & vel +~ (_accel prop)

trackPoints :: [Point] -> Int
trackPoints = fst . minimumBy (compare `on` snd) . zip [0..] . fmap l1dist
  where
    l1dist :: Point -> Int
    l1dist = sum . fmap abs . _pos

collidePoints :: [Point] -> [Point]
collidePoints = fmap head . filter (\l -> length l == 1) . group . sort

answerPoints :: ([Point] -> Int) -> ([Point] -> [Point]) -> [Point] -> [Int]
answerPoints measure update = unfoldr go
  where
    go prop = Just (measure prop, update prop)

day20answer1 = do
  input <- parseLines day20file parseInput
  return $ (!! 1000) . answerPoints trackPoints updatePoints $ input

day20answer2 = do
  input <- parseLines day20file parseInput
  return $ (!! 1000) . answerPoints length (collidePoints . updatePoints) $ input
