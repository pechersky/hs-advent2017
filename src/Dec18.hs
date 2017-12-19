module Dec18 where

import Data.List
import qualified Data.List.Split as LS
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Maybe
import Data.Either
import Control.Monad
import qualified Control.Monad.Trans.State.Strict as ST
import Data.Tuple (swap)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import AOCommon (Parser, parseLines)

day18file :: String
day18file = "src/day18input1.txt"

data CVal = Register Char | Value Integer | Stored
  deriving (Show, Eq, Ord)

data RetVal = Send CVal | Recv CVal | Jumpval CVal
  deriving Show
data Command = Sound CVal
             | Set CVal CVal
             | Add CVal CVal
             | Mul CVal CVal
             | Mod CVal CVal
             | Recover CVal
             | Jump CVal CVal
  deriving Show

type Bank = M.Map CVal CVal

parseInput :: Parser Command
parseInput = choice [
  Sound <$ string "snd " <*> pCVal,
  Set <$ string "set " <*> pRegister <* space <*> pCVal,
  Add <$ string "add " <*> pRegister <* space <*> pCVal,
  Mul <$ string "mul " <*> pRegister <* space <*> pCVal,
  Mod <$ string "mod " <*> pRegister <* space <*> pCVal,
  Recover <$ string "rcv " <*> pRegister,
  Jump <$ string "jgz " <*> pCVal <* space <*> pCVal
  ]
  where
    pRegister :: Parser CVal
    pRegister = Register <$> letterChar
    pValue :: Parser CVal
    pValue = Value <$> (L.signed space (L.decimal))
    pCVal :: Parser CVal
    pCVal = choice [pRegister, pValue]

runCommand :: Bank -> Command -> (Bank, Maybe RetVal)
runCommand !bank !cmd = case cmd of
  Sound cval -> (M.insert (Stored) (get cval) bank, Just . Send $ (get cval))
  Set rval cval -> (M.insert rval (get cval) bank, Nothing)
  Add rval cval -> (M.adjust ((.+) $ get cval) rval bank, Nothing)
  Mul rval cval -> (M.adjust ((.*) $ get cval) rval bank, Nothing)
  Mod rval cval -> (M.adjust ((flip vmod) $ get cval) rval bank, Nothing)
  Recover cval -> case (get cval) of
    Value v -> if v /= 0
               then (bank, Just . Recv $ (get Stored))
               else (bank, Nothing)
    _ -> (bank, Nothing)
  Jump rval cval -> case (get rval) of
    Value v -> if v > 0
               then (bank, Just . Jumpval $ (get cval))
               else (bank, Nothing)
  where
    get val = findBank val bank
    upd f val oldval = case oldval of
      Nothing -> Just val
      Just oval -> Just (f val oval)

findBank :: CVal -> Bank -> CVal
findBank cval bank = case cval of
  Value v -> Value v
  _ -> M.findWithDefault (Value 0) cval bank

(.+) :: CVal -> CVal -> CVal
x .+ y = case x of
  Value vx -> case y of
    Value vy -> Value (vx + vy)
    _ -> x
  _ -> x

(.*) :: CVal -> CVal -> CVal
x .* y = case x of
  Value vx -> case y of
    Value vy -> Value (vx * vy)
    _ -> x
  _ -> x

vmod :: CVal -> CVal -> CVal
vmod x y = case x of
  Value vx -> case y of
    Value vy -> Value (vx `mod` vy)
    _ -> x
  _ -> x

(!?) :: Integral a => [b] -> a -> b
(!?) l i = l !! (fromIntegral i)

runCommands :: Bank -> [Command] -> CVal
runCommands bank cmds = go bank cmds 0
  where
    go !b !cs !i = case rv of
      Nothing -> go nb cs (i + 1)
      Just (Recv cval) -> cval
      Just (Jumpval cval) -> case cval of
        Value v -> go nb cs (i + v)
        _ -> go nb cs (i + 1)
      Just _ -> go nb cs (i + 1)
      where
        (nb, rv) = runCommand b curr
        curr = cs !? i

type Queue = S.Seq CVal

runCommandM :: Bank -> Command -> ST.State (Queue, Queue) (Either CVal (Bank, Maybe RetVal))
runCommandM !bank !cmd = case cmd of
  Sound cval -> do
    ST.modify' (\(rq, sq) -> (rq, (get cval) S.<| sq))
    return $ Right (M.adjust (Value 1 .+) Stored bank, Nothing)
  Recover rval -> do
    (rq, sq) <- ST.get
    case rq of
      S.Empty -> return $ Left (get Stored)
      (q S.:|> qval) -> ST.put (q, sq) >> (return $ Right ((M.insert rval qval bank), Nothing))
  _ -> return $ Right (runCommand bank cmd)
  where
    get val = findBank val bank

runCommandsM :: [Command] -> Bank -> Integer -> Bank -> Integer
             -> ST.State (Queue, Queue) ((Either CVal (Bank, Integer)), (Either CVal (Bank, Integer)))
runCommandsM cmds !b0 !i0 !b1 !i1 = do
  let cmd0 = cmds !? i0
  let cmd1 = cmds !? i1
  resp0 <- runCommandM b0 cmd0
  ST.modify' swap
  resp1 <- runCommandM b1 cmd1
  ST.modify' swap
  case (resp0, resp1) of
    -- this is the exit case
    (Left cval0, Left cval1) -> return $ (Left cval0, Left cval1)
    (Left cval0, Right (nb1, rv1)) -> return $ (Right (b0, i0), go resp1 i1)
    (Right (nb0, rv0), Left cval1) -> return $ (go resp0 i0, Right (b1, i1))
    (Right (nb0, rv0), Right (nb1, rv1)) -> return $ (go resp0 i0, go resp1 i1)
  where
    go (Right (nb, rv)) i = case rv of
      Just (Jumpval cval) -> case cval of
        Value v -> Right (nb, (i + v))
      _ -> Right (nb, (i + 1))

stepCommands :: [Command]
             -> ST.State (Queue, Queue) ((Either CVal (Bank, Integer)), (Either CVal (Bank, Integer)))
             -> ST.State (Queue, Queue) ((Either CVal (Bank, Integer)), (Either CVal (Bank, Integer)))
stepCommands cmds s0 = do
  resp <- s0
  case resp of
    (Right (b0, i0), Right (b1, i1)) -> runCommandsM cmds b0 i0 b1 i1
    _ -> return $ resp

initState cmds = runCommandsM cmds
  (M.fromList [(Register 'p',Value 0), (Stored, Value 0)]) 0
  (M.fromList [(Register 'p',Value 1), (Stored, Value 0)]) 0

finalCommands :: [Command] -> CVal
finalCommands cmds = head $ lefts $ fmap (snd . finalCommands' cmds) [0..]

finalCommands' cmds i = (flip ST.evalState) (S.Empty, S.Empty) $ ((iterate f) x) !? i
  where
    f = stepCommands cmds
    x = initState cmds

day18answer1 = do
  input <- parseLines day18file parseInput
  return $ runCommands M.empty input

day18answer2 = do
  input <- parseLines day18file parseInput
  return $ snd $ finalCommands' input 5e5
