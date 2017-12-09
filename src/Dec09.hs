{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Dec09 where

import Data.List
import qualified Data.Text as T
import Data.Ord
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String

day09file :: String
day09file = "src/day09input1.txt"

txtdata09 :: IO String
txtdata09 = readFile day09file

type TChar = Char
type TWord = [Char]

data TGroup = TNode TChar [TGroup] TChar | TGarbage TChar TWord | TTerminal TWord
  deriving Show

test p = parse p ""

sanitize "" = ""
sanitize (a:xs)
  | a == '!' = sanitize (tail xs)
  | otherwise = a:(sanitize xs)

groupParser :: Parsec String st TGroup
groupParser = garbage <|> outgroup <|> ingroup
  where
    ingroup = TNode <$> char '{' <*> many groupParser <*> char '}'
    garbage = TGarbage <$> char '<' <*> manyTill anyChar (try (char '>')) <* notFollowedBy (char '>')
    outgroup = TTerminal <$> many1 (noneOf "{}")

numNodes :: TGroup -> Int
numNodes tgroup = case tgroup of
  TNode _ xs _ -> sum (fmap numNodes xs) + 1
  _ -> 0

scoreNodes' :: Int -> TGroup -> Int
scoreNodes' score tgroup = case tgroup of
  TNode _ xs _ -> sum ((fmap (scoreNodes' (score + 1))) xs) + (score + 1)
  _ -> 0

scoreNodes = scoreNodes' 0

minput :: IO [String]
minput = fmap (lines) txtdata09

wrongday09answer1 = do
  input <- minput
  return $ fmap (fmap numNodes . test groupParser . sanitize) $ input

wrongday09answer2 = do
  input <- minput
  return $ fmap (fmap numNodes . test groupParser
      . T.unpack
      . T.concat
      . keephead (T.dropWhile (/= '>'))
      . T.splitOn "<"
      . T.pack
      . sanitize) $ input
  where
    keephead f (x:xs) = x:(fmap f xs)

day09answer1 = do
  input <- minput
  return $ (either (const (negate 1)) id) . fmap scoreNodes . test groupParser
      . T.unpack
      . T.concat
      . keephead (T.dropWhile (/= '>'))
      . T.splitOn "<"
      . T.pack
      . sanitize
      . head $ input
  where
    keephead f (x:xs) = x:(fmap f xs)

getOneGarbage text
  | rest == "" = Nothing
  | otherwise  = Just (eg, rest)
  where
    (_, bg) = T.breakOn "<" text
    (eg, rest) = T.breakOn ">" bg
getAllGarbage text = unfoldr getOneGarbage text

day09answer2 = do
  input <- fmap (T.pack . sanitize . head) minput
  let rawgarbage = getAllGarbage input
  return $ (T.length (T.concat rawgarbage)) - (length rawgarbage)
