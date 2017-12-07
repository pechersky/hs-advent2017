{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFoldable #-}

module Dec07 where

import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Map.Strict as M

day07file :: String
day07file = "src/day07input1.txt"

txtdata07 :: IO String
txtdata07 = readFile day07file

data TowerNode = TowerNode T.Text Int [T.Text]
  deriving (Show, Eq, Ord)

data Tree a = Nil | Node a [Tree a]
  deriving (Show, Eq, Ord, Foldable)

type Tower = M.Map TowerNode [TowerNode]

minput = fmap ((fmap (finish . break)) . rows . T.pack) txtdata07
  where
    rows :: T.Text -> [T.Text]
    rows = T.lines

    {-break :: T.Text -> (T.Text, T.Text)-}
    break = T.breakOn " -> "

    finish (astr, bstr) = TowerNode (name astr) (weight astr) (children bstr)
    name astr = T.strip $ fst $ T.breakOn " " astr
    weight astr = read @Int $ T.unpack $ T.filter (`elem` digits) $ snd $ T.breakOn " " astr
    children bstr
      | bstr == "" = []
      | otherwise = tail $ T.words $ T.filter (/= ',') bstr
    digits = "0123456789" :: [Char]


buildsemiTower :: [TowerNode] -> M.Map T.Text TowerNode
buildsemiTower xs = M.fromList towerlist
  where
    towerlist = fmap (\tnode@(TowerNode name weight children) -> (name, tnode)) xs

buildTower :: M.Map T.Text TowerNode -> Tower
buildTower xmap = finalmap
  where
    nodemap :: M.Map TowerNode TowerNode
    nodemap = M.mapKeys (xmap M.!) xmap
    finalmap :: M.Map TowerNode [TowerNode]
    finalmap = M.map getChildren nodemap
    getChildren :: TowerNode -> [TowerNode]
    getChildren pnode@(TowerNode _ _ children) = fmap (xmap M.!) children

buildNodeTree :: Tower -> M.Map TowerNode (Tree (T.Text, Int))
buildNodeTree xmap = M.mapWithKey func xmap
  where
    func :: TowerNode -> [TowerNode] -> Tree (T.Text, Int)
    func tnode@(TowerNode name weight _) tnodes = Node (name, weight) (fmap go tnodes)
    go :: TowerNode -> Tree (T.Text, Int)
    go tnode = func tnode (xmap M.! tnode)

{-buildTree :: M.Map TowerNode (Tree (T.Text, Int)) -> Tree (T.Text, Int)-}
{-buildTree nodemap = M.foldrWithKey' fold_ nodemap-}
  {-where-}
    {-fold_ :: TowerNode -> [TowerNode] -> -}

extractname :: Tree (T.Text, Int) -> T.Text
extractname Nil = ""
extractname (Node (name, weight) nodes) = name

sumweight :: Tree (T.Text, Int) -> Int
sumweight Nil = 0
sumweight (Node (_, weight) nodes) = weight + sum (fmap sumweight nodes)

balanced :: Tree (T.Text, Int) -> Bool
balanced Nil = True
balanced tnode@(Node (_, weight) nodes) = length nubweights <= 1
  where
    nubweights = nub (nodeweights tnode)

balancedChildren :: Tree (T.Text, Int) -> [Bool]
balancedChildren Nil = [True]
balancedChildren tnode@(Node (_, weight) nodes) = (fmap balanced nodes)

nodeweights :: Tree (T.Text, Int) -> [Int]
nodeweights Nil = []
nodeweights (Node (_, weight) nodes) = fmap sumweight nodes

nodeweightsAndSelf :: Tree (T.Text, Int) -> (Int, [Int])
nodeweightsAndSelf Nil = (0, [])
nodeweightsAndSelf (Node (_, weight) nodes) = (weight, fmap sumweight nodes)

mismatches :: [Tree (T.Text, Int)] -> [Tree (T.Text, Int)]
mismatches = filter (not . balanced)

selfweight :: Tree (T.Text, Int) -> Int
selfweight Nil = 0
selfweight tnode@(Node (_, weight) _) = weight

{-shiftedweight :: Tree (T.Text, Int) -> Int-}
shiftedweight Nil = (0, 0)
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
  let semitower = buildsemiTower input
  let tower = buildTower semitower
  let treemap = buildNodeTree tower
  let bigtree = maximumBy (comparing length) $ M.elems treemap
  return $ extractname bigtree

day07answer2 = do
  input <- minput
  let semitower = buildsemiTower input
  let tower = buildTower semitower
  let treemap = buildNodeTree tower
  let answer = snd $ minimumBy (comparing fst) $ fmap shiftedweight $ mismatches $ M.elems treemap
  return answer
