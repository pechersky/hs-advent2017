{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Dec07 where

import Data.List
import Data.Ord
import Data.Char
import Data.Tree (Tree (Node))
import qualified Data.Tree as TR
import qualified Data.Text as T
import qualified Data.Map.Strict as M

day07file :: String
day07file = "src/day07input1.txt"

txtdata07 :: IO String
txtdata07 = readFile day07file

data TextNode = TextNode {_tnname :: String, _tnweight :: Int, _tnchildren :: [String]}
  deriving (Show, Eq, Ord)

type TextTower = M.Map TextNode [TextNode]
type Disc = Tree Int

_weight :: Disc -> Int
_weight = TR.rootLabel

_nodes :: Disc -> [Disc]
_nodes = TR.subForest

minput :: IO [TextNode]
minput = fmap ((fmap (finish . clean)) . lines) txtdata07
  where
    clean = filter (\c -> isAlphaNum c || c == ' ')
    finish (words->(name:weight:children)) = TextNode name (read @Int weight) children

buildNodeTree :: [TextNode] -> M.Map String Disc
buildNodeTree xs = M.mapWithKey (\k _ -> get k) xmap
  where
    towerlist :: [(String, TextNode)]
    towerlist = fmap (\tnode -> (_tnname tnode, tnode)) xs

    xmap :: M.Map String TextNode
    xmap = M.fromList towerlist

    getNode :: String -> TextNode
    getNode = (xmap M.!)

    getChildren :: TextNode -> [TextNode]
    getChildren pnode = fmap getNode (_tnchildren pnode)

    expand :: String -> (Int, [String])
    expand nodename = (_tnweight tnode, _tnchildren tnode)
      where
        tnode = xmap M.! nodename

    get :: String -> Disc
    get nodename = TR.unfoldTree expand nodename

-- some mutual recursion going on here
sumweight :: Disc -> Int
sumweight node = _weight node + sum (nodeweights node)

nodeweights :: Disc -> [Int]
nodeweights = fmap sumweight . _nodes

balanced :: Disc -> Bool
balanced tnode = length nubweights <= 1
  where
    nubweights = nub (nodeweights tnode)

mismatches :: [Disc] -> [Disc]
mismatches = filter (not . balanced)

shiftedweight :: Disc -> (Int, Int)
shiftedweight tnode = shift
  where
    sumw = nodeweights tnode
    nweights = fmap _weight (_nodes tnode)
    diffweight = (maximum sumw) - (minimum sumw)
    uniques _ [] = []
    uniques f (x:xs)
      | f x `elem` fmap f (uniques f xs) = [y | y <- uniques f xs, f y /= f x]
      | otherwise = x : uniques f xs
    fixweight (child, self) = (child, self - diffweight)
    shift = (head . fmap fixweight . uniques fst) $ zip sumw nweights

day07answer1 = do
  input <- minput
  let treemap = buildNodeTree input
  let bigtree = (fst . maximumBy (comparing (length . snd)) . M.toList) treemap
  return $ bigtree

day07answer2 = do
  input <- minput
  let treemap = buildNodeTree input
  let answer = (snd . minimumBy (comparing fst) . fmap shiftedweight . mismatches . M.elems) treemap
  return answer
