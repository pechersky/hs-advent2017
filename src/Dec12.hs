module Dec12 where

import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S

day12file :: String
day12file = "src/day12input1.txt"

txtdata12 :: IO String
txtdata12 = readFile day12file

minput :: IO [[String]]
minput = fmap (fmap (words) . lines) txtdata12

parseInput :: [String] -> (Int, [Int])
parseInput (ix:_:xs) = (read ix, fmap (read . filter isDigit) xs)

connections :: [(Int, [Int])] -> M.Map Int [Int]
connections = M.fromList

collect val connect = unfoldr go (S.singleton val)
  where
  go :: S.Set Int -> Maybe (S.Set Int, S.Set Int)
  go keys
    {-| S.size diffkeys == 0 = Nothing-}
    | otherwise = Just (keys, newkeys)
    where
      newkeys = S.fromList (S.toList keys ++ (concatMap (connect M.!) keys))
      diffkeys = newkeys S.\\ keys

endGroup colllist = fst . head . dropWhile (\(xs, ys) -> S.size xs /= S.size ys) $ zip colllist (drop 1 colllist)

day12answer1 = do
  input <- minput
  let group = collect 0 . connections . fmap parseInput $ input
  return $ S.size . endGroup $ group

day12answer2 = do
  input <- minput
  let startMap = connections . fmap parseInput $ input
  let groupMap = M.mapWithKey (\k _ -> endGroup $ collect k startMap) startMap
  return $ S.size . S.fromList . M.elems $ groupMap
