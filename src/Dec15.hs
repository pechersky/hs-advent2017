module Dec15 where

import Data.Bits

input :: (Integer, Integer)
{-input = (65, 8921)-}
input = (591, 393)

step :: (Integer, Integer) -> (Integer, Integer)
step (x,y) = ((x*16807) `rem` val, (y*48271) `rem` val)
  where
    val = 2147483647

check :: (Integer, Integer) -> Bool
check (x,y) = all id [testBit x i == testBit y i | i <- [0..15]]

newlists :: [(Integer, Integer)]
newlists = zip (((filter ((== 0) . (flip rem) 4)) . fmap fst) list) (((filter ((== 0) . (flip rem) 8)) . fmap snd) list)
  where
    list = iterate step input

day15answer1 = length . filter check . take 40000000 . iterate step $ input

day15answer2 = length . filter check . take 5000000 $ newlists
