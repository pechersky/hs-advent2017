module Dec03 where

import Data.List
import qualified Data.Map.Strict as M

type Pos = (Integer, Integer)

spiralmove :: Int -> [Pos]
spiralmove x = (qrep mr x 1) ++ (qrep mu x 1) ++ (qrep ml x 2) ++ (qrep md x 2) ++ spiralmove (x + 1)
  where
    qrep :: Pos -> Int -> Int -> [Pos]
    qrep move val aval = replicate (2 * val + aval) move
    mr = (1, 0)
    mu = (0, 1)
    ml = (-1, 0)
    md = (0, -1)

summoves :: [Pos]
summoves = scanl' (\(a,b) (c,d) -> (a+c,b+d)) (0,0) moves
  where
    moves = spiralmove 0

spiralpos :: Int -> Pos
spiralpos x = summoves !! (x - 1)

l1dist :: Pos -> Pos -> Integer
l1dist (x0, y0) (x1, y1) = abs (y1 - y0) + abs (x1 - x0)

day03answer1 :: Integer
day03answer1 = l1dist (spiralpos 1) (spiralpos 277678)

type Grid = M.Map Pos Integer

builtgrids :: [Grid]
builtgrids = scanl mupdate sgrid summoves
  where
    mfind :: Grid -> Pos -> Integer
    mfind grid pos = sum $ fmap (flip (M.findWithDefault 0) grid) $ choosepos pos

    mupdate :: Grid -> Pos -> Grid
    mupdate grid pos = M.insert pos (mfind grid pos) grid

    sgrid :: Grid
    sgrid = M.singleton (0,0) 1

    choosepos :: Pos -> [Pos]
    choosepos (a,b) = [(f a, g b) | f <- funcs, g <- funcs]
      where
        funcs = [(subtract 1), id, (+ 1)]

maxgridvals :: [Integer]
maxgridvals = fmap (M.foldr max 0) builtgrids

day03func2 val = head $ filter (> val) maxgridvals
day03answer2 = day03func2 277678
