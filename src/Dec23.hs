{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Dec23 where

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict as ST
import Data.Maybe
import Data.Tree
import Data.IORef
import Control.Lens

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseLines)

day23file :: String
day23file = "src/day23input1.txt"

data Command = Set CVal CVal
             | Sub CVal CVal
             | Mul CVal CVal
             | Jnz CVal CVal
             | Terminated
  deriving Show

data CVal = Register Char | Value Int
  deriving (Eq, Ord)

instance Show (CVal) where
  show (Register c) = show c
  show (Value v) = show v

type Bank = M.Map CVal CVal

parseInput :: Parser Command
parseInput = choice [
  Set <$ string "set " <*> pRegister <* space <*> pCVal,
  Sub <$ string "sub " <*> pRegister <* space <*> pCVal,
  Mul <$ string "mul " <*> pRegister <* space <*> pCVal,
  Jnz <$ string "jnz " <*> pCVal <* space <*> pCVal
  ]
  where
    pRegister :: Parser CVal
    pRegister = Register <$> letterChar
    pValue :: Parser CVal
    pValue = Value <$> (L.signed space (L.decimal))
    pCVal :: Parser CVal
    pCVal = choice [pRegister, pValue]

findBank :: CVal -> Bank -> CVal
findBank cval bank = case cval of
  Value v -> Value v
  _ -> M.findWithDefault (Value 0) cval bank

(.-) :: CVal -> CVal -> CVal
x .- y = case x of
  Value vx -> case y of
    Value vy -> Value (vy - vx)
    _ -> x
  _ -> x

(.*) :: CVal -> CVal -> CVal
x .* y = case x of
  Value vx -> case y of
    Value vy -> Value (vx * vy)
    _ -> x
  _ -> x

runcmd cmds !bank !pos !acc = case cmd of
  Terminated -> [prepval]
  Set rval cval -> prepval : runcmd cmds (M.insert rval (get cval) bank) (pos + 1) acc
  Sub rval cval -> prepval : runcmd cmds (M.adjust ((.-) $ get cval) rval bank) (pos + 1) acc
  Mul rval cval -> prepval : runcmd cmds (M.adjust ((.*) $ get cval) rval bank) (pos + 1) (acc + 1)
  Jnz rval cval -> case (get rval) of
    Value v -> if v /= 0
              then prepval : runcmd cmds (bank) (pos + ex (get cval)) acc
              else prepval : runcmd cmds (bank) (pos + 1) acc
  where
    cmd = cmds !? pos
    prepval = (acc, bank)
    get val = findBank val bank
    ex :: CVal -> Int
    ex cval = case cval of
      Value v -> v
      _ -> 1

(!?) :: V.Vector Command -> Int -> Command
cmds !? pos = case cmds V.!? pos of
  Nothing -> Terminated
  Just cmd -> cmd

primes = 2 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 3 : filter (isPrime primes') [5, 7 ..]

day23answer1 = do
  input <- V.fromList <$> parseLines day23file parseInput
  return $ fst . last $ (runcmd input M.empty 0 0)

day23answer2 = do
  input <- V.fromList <$> parseLines day23file parseInput
  let intbank = snd . (!! 100) $ (runcmd input (M.singleton (Register 'a') (Value 1)) 0 0)
      ex cval = case cval of
        Value v -> v
        _ -> 1
      b = ex $ intbank M.! (Register 'b')
      c = ex $ intbank M.! (Register 'c')
      incr = 17 -- from line 31 in input
      intprimes = takeWhile (< c) . dropWhile (< b) $ primes
      intnums = [b, b+incr..c]
      composites = S.difference (S.fromList intnums) (S.fromList intprimes)
  return $ length $ composites


-- debugging and lens-style inspection below

compile cmds = unfoldTreeM_BF go (1, M.empty)
  where
    go :: (Int, Bank) -> [((Int, Command, Bank), [(Int, Bank)])]
    go (pos, bank) = do
      let restpos = case cmd of
                      Terminated -> []
                      Set rval cval -> [(pos + 1, M.insert rval (get cval) bank)]
                      Sub rval cval -> [(pos + 1, M.adjust ((.-) $ get cval) rval bank)]
                      Mul rval cval -> [(pos + 1, M.adjust ((.*) $ get cval) rval bank)]
                      Jnz rval cval -> case rval of
                        Value v -> if v /= 0
                                  then let cv = ex (get cval) in if cv > 0
                                    then [(pos + ex (get cval), bank)]
                                    else [(pos + 1, bank)]
                                  else [(pos + 1, bank)]
                        (get->Value v) -> if v /= 0 || True -- want to see branches
                                  then let cv = ex (get cval) in if cv > 0
                                    then [(pos + 1, M.insert rval (Value 0) bank), (pos + ex (get cval), bank)]
                                    else [(pos + 1, M.insert rval (Value 0) bank)]
                                  else [(pos + 1, bank)]
      return $ ((pos, cmd, bank), restpos)
        where
          cmd = cmds !? (pos - 1)
          get val = findBank val bank
          ex :: CVal -> Int
          ex cval = case cval of
            Value v -> v
            _ -> 1

day23tree = do
  input <- V.fromList <$> parseLines day23file parseInput
  mapM_ putStrLn $ fmap (drawTree . fmap show) . compile $ input

data RegState = RS {_a :: Int, _b :: Int, _c :: Int, _d :: Int,
                    _e :: Int, _f :: Int, _g :: Int, _h :: Int,
                    _ptr :: Int, _acc :: Int,
                    _dcmds :: [(Command, Int, Int, Int, Int, Int, Int, Int, Int)]}
  deriving (Show)
makeLenses ''RegState

readCommand :: Monad a => Command -> ST.StateT RegState a ()
readCommand cmd = case cmd of
  Terminated -> return ()
  Set rval cval -> do
    vv <- pUse cval
    lview rval .= vv
    ptr += 1
    after
  Sub rval cval -> do
    vv <- pUse cval
    lview rval %= (subtract vv)
    ptr += 1
    after
  Mul rval cval -> do
    vv <- pUse cval
    lview rval %= (* vv)
    ptr += 1
    after
    rg <- get
    dcmds %= ((cmd,rg^.a,rg^.b,rg^.c,rg^.d,rg^.e,rg^.f,rg^.g,rg^.h):)
  Jnz rval cval -> do
    vv <- pUse cval
    vr <- pUse rval
    if vr /= 0
      then ptr += vv
      else ptr += 1
    after
  where
    pUse rval = case rval of
      Value v -> return (v)
      Register r -> case r of
        'a' -> use a
        'b' -> use b
        'c' -> use c
        'd' -> use d
        'e' -> use e
        'f' -> use f
        'g' -> use g
        'h' -> use h
        _ -> error "missing use register"
    lview rval = case rval of
      Register r -> case r of
        'a' -> a
        'b' -> b
        'c' -> c
        'd' -> d
        'e' -> e
        'f' -> f
        'g' -> g
        'h' -> h
        _ -> error "missing view register"
    after = do
      acc += 1

primeCommand :: Monad a => ST.StateT RegState a ()
primeCommand = do
  vb <- use b
  d .= vb
  e .= vb
  g .= 0
  if vb `elem` (takeWhile (<= vb) primes)
    then do {return ()}
    else do {f .= 0}
  return ()

day23bystate1 = do
  input <- parseLines day23file parseInput
  let lcmds = fmap readCommand input
  let sstate = RS 0 0 0 0 0 0 0 0 0 0 []
  let cmds = do
              whileM_ (do
                vptr <- use ptr
                vacc <- use acc
                return (vptr >= 0 && vptr < length lcmds)) $ do
                  vptr <- use ptr;
                  if False
                    then do {primeCommand; ptr += 15}
                    else lcmds !! vptr
              vh <- use h
              return $ vh
  let rstate = execState cmds sstate
  return $ length $ view dcmds rstate

day23bystate2 = do
  input <- parseLines day23file parseInput
  let lcmds = fmap readCommand input
  let sstate = RS 1 0 0 0 0 0 0 0 0 0 []
  let cmds = do
              whileM_ (do
                vptr <- use ptr
                vacc <- use acc
                return (vptr >= 0 && vptr < length lcmds)) $ do
                  vptr <- use ptr;
                  if vptr == 9
                    then do {primeCommand; ptr += 15}
                    else lcmds !! vptr
              vh <- use h
              return $ vh
  let rstate = execState cmds sstate
  return $ view h rstate
