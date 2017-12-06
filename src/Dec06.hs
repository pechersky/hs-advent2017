{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Dec06 where

import Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as M

day06file :: String
day06file = "src/day06input1.txt"

txtdata06 :: IO String
txtdata06 = readFile day06file

-- still using M.Map.Strict because it seems fastest and simplest
memory = fmap (ixlist . intarr . head . cell . rows . T.pack) txtdata06
  where
    rows :: T.Text -> [T.Text]
    rows = T.lines

    cell :: [T.Text] -> [[T.Text]]
    cell = fmap T.words

    intarr :: [T.Text] -> [Int]
    intarr = fmap (read . T.unpack)

ixlist :: [Int] -> M.Map Int Int
ixlist = M.fromList . zip [0::Int ..]

-- rely on the fact that @M.toList@ will use the implied @Ord k@ ordering
-- use @M.foldlWithKey@ to make sure it is leftmost (lowest) key
getMax :: Ord a => M.Map k a -> (k,a)
getMax xmap = M.foldlWithKey keyCompare ((head . M.toList) xmap) xmap
  where
    keyCompare (ok, oa) k a
      | oa < a = (k, a)
      | otherwise = (ok, oa)

redistribute :: M.Map Int Int -> M.Map Int Int
redistribute oldmap = newmap
  where
    newmap = fold_ (const (+1)) (M.adjust (const 0) pos oldmap) keys
    (pos, val) = getMax oldmap
    fold_ = foldr . M.adjustWithKey
    keys = [k `mod` M.size oldmap | (k, v) <- zip [(pos + 1) ..] [1 .. val]]

-- use just the values for comparison for quick/cheap as opposed to comparing Maps
type Output = [Int]
expandstates :: M.Map Int Int -> [Output]
expandstates xmap = go [] xmap
  where
    go :: [Output] -> M.Map Int Int -> [Output]
    go !paststates !oldmap
      | state `elem` paststates = paststates ++ [state]
      | otherwise = go (paststates ++ [state]) newmap
        where
          newmap = redistribute oldmap
          state = (M.elems oldmap)

day06answer1 = do
  xmap <- memory
  return $ (subtract 1) . length $ expandstates xmap

day06answer2 = do
  xmap <- memory
  let statelist = expandstates xmap
  let laststate = head (reverse statelist)
  return $ (subtract 1) . length $ dropWhile (/= laststate) statelist
