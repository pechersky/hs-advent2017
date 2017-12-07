{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Dec07 where

import Data.List
import Data.Ord
import Data.Tree (Tree (Node))
import qualified Data.Tree as TR
import qualified Data.Text as T
import qualified Data.Map.Strict as M

day07file :: String
day07file = "src/day07input1.txt"

txtdata07 :: IO String
txtdata07 = readFile day07file

data TextNode = TextNode {_tnname :: T.Text, _tnweight :: Int, _tnchildren :: [T.Text]}
  deriving (Show, Eq, Ord)

type TextTower = M.Map TextNode [TextNode]
type Disc = Tree (T.Text, Int)

-- could probably improve this with lenses
_name :: Disc -> T.Text
_name (Node (name, _) _) = name

_weight :: Disc -> Int
_weight (Node (_, weight) _) = weight

_nodes :: Disc -> [Disc]
_nodes (Node (_, _) nodes) = nodes

minput :: IO [TextNode]
minput = fmap ((fmap (finish . break)) . rows . T.pack) txtdata07
  where
    rows :: T.Text -> [T.Text]
    rows = T.lines

    break = T.breakOn " -> "

    finish (astr, bstr) = TextNode (pname astr) (pweight astr) (pchildren bstr)
    pname astr = T.strip $ fst $ T.breakOn " " astr
    pweight astr = read @Int $ T.unpack $ T.filter (`elem` digits) $ snd $ T.breakOn " " astr
    pchildren bstr
      | bstr == "" = []
      | otherwise = tail $ T.words $ T.filter (/= ',') bstr
    digits = "0123456789" :: [Char]

buildNodeTree :: [TextNode] -> M.Map T.Text Disc
buildNodeTree xs = M.mapWithKey (\k _ -> get k) xmap
  where
    towerlist :: [(T.Text, TextNode)]
    towerlist = fmap (\tnode -> (_tnname tnode, tnode)) xs

    xmap :: M.Map T.Text TextNode
    xmap = M.fromList towerlist

    getNode :: T.Text -> TextNode
    getNode = (xmap M.!)

    getChildren :: TextNode -> [TextNode]
    getChildren pnode = fmap getNode (_tnchildren pnode)

    expand :: T.Text -> ((T.Text, Int), [T.Text])
    expand nodename = ((_tnname tnode, _tnweight tnode), (_tnchildren tnode))
      where
        tnode = xmap M.! nodename

    get :: T.Text -> Disc
    get nodename = TR.unfoldTree expand nodename

sumweight :: Disc -> Int
sumweight node = _weight node + sum (fmap sumweight (_nodes node))

balanced :: Disc -> Bool
balanced tnode@(Node (_, weight) nodes) = length nubweights <= 1
  where
    nubweights = nub (nodeweights tnode)

balancedChildren :: Disc -> [Bool]
balancedChildren tnode@(Node (_, weight) nodes) = (fmap balanced nodes)

nodeweights :: Disc -> [Int]
nodeweights (Node (_, weight) nodes) = fmap sumweight nodes

nodeweightsAndSelf :: Disc -> (Int, [Int])
nodeweightsAndSelf (Node (_, weight) nodes) = (weight, fmap sumweight nodes)

mismatches :: [Disc] -> [Disc]
mismatches = filter (not . balanced)

selfweight :: Disc -> Int
selfweight tnode@(Node (_, weight) _) = weight

{-shiftedweight :: Disc -> Int-}
shiftedweight tnode@(Node (_, weight) nodes) = shift
  where
    sumw = nodeweights tnode
    nweights = fmap selfweight nodes
    diffweight = (maximum sumw) - (minimum sumw)
    uniques _ [] = []
    uniques f (x:xs)
      | f x `elem` fmap f (uniques f xs) = [y | y <- uniques f xs, f y /= f x]
      | otherwise = x : uniques f xs
    {-shift = fmap (subtract diffweight . snd) $ uniques fst (zip sumw nweights)-}
    shift = head $ fmap (\(cweight, selfweight) -> (cweight, selfweight - diffweight)) $ uniques fst (zip sumw nweights)

day07answer1 = do
  input <- minput
  let treemap = buildNodeTree input
  let bigtree = maximumBy (comparing length) $ M.elems treemap
  return $ _name bigtree

day07answer2 = do
  input <- minput
  let treemap = buildNodeTree input
  let answer = snd $ minimumBy (comparing fst) $ fmap shiftedweight $ mismatches $ M.elems treemap
  return answer
