module Dec19 where

import Data.List
import qualified Data.Set as S

import Linear.V2
import Data.Maybe

day19file :: String
day19file = "src/day19input1.txt"

neighbors (V2 x y) = S.fromList [V2 (x - 1) y, V2 (x + 1) y, V2 x (y - 1), V2 x (y + 1)]

(!) :: [[a]] -> V2 Int -> Maybe a
(!) arr (V2 x y)
  | x < 0 = Nothing
  | y < 0 = Nothing
  | x >= (length $ head arr) = Nothing
  | y >= (length arr) = Nothing
  | otherwise = Just $ (arr !! y) !! x

locate = (\x -> V2 x 0 ) . fromJust . findIndex (== '|') . head

go array spos = schar : loop (spos - (V2 0 1)) spos
  where
    schar = array ! spos
    loop ppos cpos = case go' ppos cpos of
      Nothing -> []
      Just (nchar, npos) -> nchar : (loop cpos npos)
    go' prevpos startpos = case nextchar of
      Nothing -> if null otherposset then Nothing else Just (otherchar, otherpos)
      Just ' ' -> if null otherposset then Nothing else Just (otherchar, otherpos)
      _ -> Just (nextchar, nextpos)
      where
        nextpos = 2 * startpos - prevpos
        nextchar = array ! nextpos
        otherneighbors = S.difference (neighbors startpos) (S.fromList [prevpos, nextpos])
        otherposset = S.filter ((/= ' ') . (maybe ' ' id) . (array !)) otherneighbors
        otherpos = S.elemAt 0 otherposset
        otherchar = array ! otherpos

day19answer1 = do
  input <- fmap lines $ readFile day19file
  let spos = locate input
  return $ filter (\c -> c >= 'A' && c <= 'Z') . fmap fromJust $ go input spos

day19answer2 = do
  input <- fmap lines $ readFile day19file
  let spos = locate input
  return $ length . fmap fromJust $ go input spos
