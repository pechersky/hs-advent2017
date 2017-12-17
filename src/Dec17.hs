module Dec17 where

import Data.List
import Data.Maybe

movelen = 356

runSpinlock= iterate (go movelen)
  where
    go move ((!list, !pos), !inv) = ((newlist, newpos), inv+1)
      where
        lenlist = inv
        splitpos = (pos + move) `mod` lenlist
        (axs,bxs) = splitAt (splitpos + 1) list
        newlist = axs ++ [inv] ++ bxs
        newpos = splitpos + 1

shortSpinlock :: [Int] -> (Int, Int)
shortSpinlock = foldl' (shortgo movelen) (0, 1)
  where
    shortgo move !(!prevpos, !pos) lenlist
      | newpos == 1 = (lenlist, newpos)
      | otherwise = (prevpos, newpos)
      where
        splitpos = (pos + move) `mod` lenlist
        newpos = splitpos + 1

day17answer1 = spinlock!! (1 + fromJust (elemIndex 2017 spinlock))
  where
    spinlockStates = runSpinlock (([0], 0), 1)
    spinlock = (fst . fst) (spinlockStates !! 2017)

day17answer2 = fst $ shortSpinlock [1..1+5e7]
