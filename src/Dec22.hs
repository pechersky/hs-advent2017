{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Dec22 where

import Data.List
import qualified Data.Map as M

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseLines)

import Linear.V2
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Trans.State as ST
import Control.Monad.Trans.Class
import Control.Lens

day22file :: String
day22file = "src/day22input1.txt"

data Node = Infected | Clean | Weakened | Flagged
  deriving Eq
instance Show Node where
  show = \case
    Infected -> "#"
    Clean -> "."
    Weakened -> "W"
    Flagged -> "F"

data Zipper a = Zipper [a] a Int [a]
  deriving Functor
instance (Show a) => Show (Zipper a) where
  show (Zipper ls x n rs) =
    concatMap show (reverse (take 3 ls)) ++ " " ++ show (x,n) ++ " " ++ concatMap show (take 3 rs)

back (Zipper (l:ls) x n rs) = Zipper ls l (n-1) (x:rs)
forth (Zipper ls x n (r:rs)) = Zipper (x:ls) r (n+1) rs

newtype Grid a = Grid (Zipper (Zipper a))
  deriving Functor

instance Show a => Show (Grid a) where
  show (Grid (Zipper ls x n rs)) =
    unlines $ zipWith (\a b -> a ++ " " ++ b)
              (fmap show [n-3..n+3])
              (fmap show (reverse (take 3 ls) ++ [x] ++ (take 3 rs)))

data Direction = N | E | S | W
  deriving Show

class PGrid f where
  moveGrid :: Direction -> f a -> f a
  gset :: a -> f a -> f a
  gget :: f a -> a
  moveTo :: V2 Int -> f a -> f a

instance PGrid Grid where
  moveGrid = flip moveGrid'
    where
      moveGrid' (Grid g) = \case
        N -> Grid (back g)
        S -> Grid (forth g)
        E -> Grid (fmap forth g)
        W -> Grid (fmap back g)

  gset :: a -> Grid a -> Grid a
  gset val (Grid (Zipper ls row n rs)) = (Grid (Zipper ls (set' row) n rs))
    where
      set' (Zipper ls' x m rs') = Zipper ls' val m rs'

  gget :: Grid a -> a
  gget (Grid (Zipper _ (Zipper _ x _ _) _ _)) = x

  moveTo :: V2 Int -> Grid a -> Grid a
  moveTo (V2 x y) g@(Grid (Zipper _ (Zipper _ _ m _) n _))
    | n > y = moveTo (V2 x y) (moveGrid N g)
    | n < y = moveTo (V2 x y) (moveGrid S g)
    | m > x = moveTo (V2 x y) (moveGrid W g)
    | m < x = moveTo (V2 x y) (moveGrid E g)
    | otherwise = g


cleanGrid :: Grid Node
cleanGrid = Grid (Zipper cleanRows cleanRow 0 cleanRows)
  where
    cleans = repeat Clean
    cleanRow = Zipper cleans Clean 0 cleans
    cleanRows = repeat cleanRow

readVec :: Grid a -> (V.Vector (V.Vector a)) -> Grid a
readVec grid vec = foldr moveGrid (foldl' go grid vec) (replicate ly N)
  where
    ly = length vec
    lx = length (vec V.! 0)
    go :: Grid a -> V.Vector a -> Grid a
    go grid vec = moveGrid S $ foldr moveGrid (foldl' go' grid vec) (replicate lx W)
    go' grid = moveGrid E . flip gset grid

startPos :: V.Vector (V.Vector a) -> V2 Int
startPos vec = V2 x y
  where
    y = length vec `div` 2
    x = length (vec V.! 0) `div` 2

virusStep = \case
  Infected -> turnRight
  Clean -> turnLeft
  Weakened -> id
  Flagged -> reverseDir
  where
    turnRight = \case
      N -> E
      E -> S
      S -> W
      W -> N
    turnLeft = \case
      N -> W
      W -> S
      S -> E
      E -> N
    reverseDir = \case
      N -> S
      W -> E
      S -> N
      E -> W

virusSwap = \case
  Infected -> Clean
  Clean -> Infected

virusSwap2 = \case
  Clean -> Weakened
  Weakened -> Infected
  Infected -> Flagged
  Flagged -> Clean

virusScanner :: ST.StateT (Grid Node, Direction) [] Node
virusScanner = do
  (grid, dir) <- get
  let node = gget grid
      ndir = virusStep node dir
      nnode = virusSwap node
      ngrid = moveGrid ndir $ gset nnode grid
  put (ngrid, ndir)
  return nnode

data MGrid a = MGrid {_mgrid :: !(M.Map (V2 Int) a), _mpos :: !(V2 Int), _mdir :: !Direction}
makeLenses ''MGrid

moveDir dir pos = pos - shift
  where
    shift = case dir of
      N -> V2 0 1
      W -> V2 1 0
      S -> V2 0 (-1)
      E -> V2 (-1) 0

-- a plain unfoldr seems to be less memory and cpu heavy than a StateT s [] a
virusScannerM = unfoldr go
  where
    go !grid = Just (nnode, MGrid ngrid newpos ndir)
      where
        node = M.findWithDefault Clean (_mpos grid) (_mgrid grid)
        ndir = virusStep node (_mdir grid)
        newpos = moveDir ndir (_mpos grid)
        nnode = virusSwap2 node
        ngrid = case nnode of
          Clean -> M.delete (_mpos grid) (_mgrid grid) -- crucial for keeping memory down
          _ -> M.insert (_mpos grid) nnode (_mgrid grid)

readVecM :: (V.Vector (V.Vector a)) -> [(V2 Int, a)]
readVecM vec = concatMap go $ zip (V.toList vec) [0..]
  where
    go :: (V.Vector a, Int) -> [(V2 Int, a)]
    go (svec, y) = [(V2 x y, item) | (item, x) <- zip (V.toList svec) [0..]]

parseInput :: Parser (V.Vector Node)
parseInput = V.fromList <$> many (choice [infected, clean])
  where
    infected = Infected <$ char '#'
    clean = Clean <$ char '.'

day22answer1 = do
  input <- V.fromList <$> parseLines day22file parseInput
  let spos = startPos input
      startGrid = moveTo spos . readVec cleanGrid $ input
  return $ length . filter (== Infected) . head $ evalStateT (replicateM 10000 virusScanner) (startGrid, N)

day22answer2 = do
  input <- V.fromList <$> parseLines day22file parseInput
  let spos = startPos input
      startGridM = M.fromList . readVecM $ input
  return $ length . filter (== Infected) . take 1e7 . virusScannerM $ MGrid startGridM spos N
