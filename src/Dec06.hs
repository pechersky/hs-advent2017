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

tostr :: [Int] -> String
tostr = (intercalate " " . fmap show)

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

expandstates :: M.Map Int Int -> [String]
expandstates xmap = go startval startpos [] xmap
  where
    (startpos, startval) = getMax xmap

    go :: Int -> Int -> [String] -> M.Map Int Int -> [String]
    go !val !pos !pastlists !list
      | strlist `elem` pastlists = pastlists ++ [strlist]
      | otherwise = go newval newpos (pastlists ++ [strlist]) newlist
        where
          newlist = loop val pos (M.adjust (const 0) pos list)
          (newpos, newval) = getMax newlist
          strlist = tostr (M.elems list)

          loop :: Int -> Int -> M.Map Int Int -> M.Map Int Int
          loop !ival !ipos !ilist
            | ival == 0 = ilist
            | otherwise = loop (ival - 1) jpos (M.adjust (+1) jpos ilist)
            where
              jpos = (ipos + 1) `mod` M.size ilist


day06answer1 = do
  xmap <- memory
  let (pos, val) = getMax xmap
  return $ (subtract 1) . length $ expandstates xmap

day06answer2 = do
  xmap <- memory
  let (pos, val) = getMax xmap
  let statelist = expandstates xmap
  let laststate = head (reverse statelist)
  return $ (subtract 1) . length $ dropWhile (/= laststate) statelist
