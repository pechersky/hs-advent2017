module Dec17 where

import Data.List
import Data.Maybe

movelen = 356

go move !list !pos !inv = ((newlist, newpos), inv+1)
  where
    lenlist = inv
    splitpos = (pos + move) `mod` lenlist
    (axs,bxs) = splitAt (splitpos + 1) list
    newlist = axs ++ [inv] ++ bxs
    newpos = splitpos + 1

itf = iterate (uncurry (uncurry (go movelen)))

x = itf (([0], 0), 1)

y = fst . fst $ x !! 2017


itf2 :: [Int] -> (Int, Int)
itf2 = foldl' (shortgo movelen) (0, 1)
  where
    shortgo move !(!prevpos, !pos) lenlist
      | newpos == 1 = (lenlist, newpos)
      | otherwise = (prevpos, newpos)
      where
        splitpos = (pos + move) `mod` lenlist
        newpos = splitpos + 1

day17answer1 = y !! (1 + fromJust (elemIndex 2017 y))

day17answer2 = fst $ itf2 [1..1+5e7]
