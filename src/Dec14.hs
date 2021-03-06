module Dec14 where

import Data.List
import Data.Char
import Numeric
import Data.Maybe
import Data.Graph

import Dec10 (knothash)

input :: String
{-input = "flqrgnkx"-}
input = "hxtvlmkl"

hash :: String -> [[Char]]
hash inp = fmap (concat . fmap (toBits . \a -> [a]) . knothash . parthash) $ zip (repeat inp) [0..127]

parthash :: (String, Int) -> String
parthash (inp,int) = inp ++ "-" ++ show int

pad :: Char -> Int -> String -> String
pad c l str = replicate (l - length str) '0' ++ str

toBits :: String -> String
toBits str = pad '0' 4 $ showIntAtBase 2 intToDigit int ""
  where
    int = (fst . head . readHex) str

type Index = Int
type Pos = (Int, Int)

travel :: Int -> [Pos]
travel maxval = concat [[(x,y-x) | x <- [0..y], x <= maxval, (y-x) <= maxval] | y <- [0..2*maxval]]

nodes :: Int -> [(Index, Pos)]
nodes maxval = zip [0..] (travel maxval)

gnodes :: Int -> [[Char]] -> [Maybe (Index, Pos, [Pos])]
gnodes maxval grid = fmap go (nodes maxval)
  where
    go (i,(x,y))
      | grid !! y !! x == '0' = Nothing
      | otherwise = Just (i, (x,y), keepnodes (x,y))
    neighbors (x,y) = filter (\(vx,vy) -> vx >= 0 && vx <= maxval && vy >= 0 && vy <= maxval) [(x,y), (x+1,y), (x-1,y), (x,y+1), (x,y-1)]
    keepnodes (x,y) = filter (\(vx,vy) -> (grid !! vy !! vx) /= '0') (neighbors (x,y))

day14answer1 = sum $ fmap (length . filter (== '1')) $ hash input

day14answer2 = length . stronglyConnComp . catMaybes $ gnodes 127 $ hash input
