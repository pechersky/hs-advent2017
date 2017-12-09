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

data TGroup = TNode [TGroup] | TGarbage [TGroup] | TTerminal TChar
  deriving Show

applyP :: Parsec String () a -> String -> Either ParseError a
applyP p = parse p ""

sanitize :: String -> String
sanitize "" = ""
sanitize (a:xs)
  | a == '!' = sanitize (tail xs)
  | otherwise = a:(sanitize xs)

groupParser :: Parsec String st TGroup
groupParser = TGarbage  <$> garbage
          <|> TTerminal <$> outgroup
          <|> TNode     <$> ingroup
  where
    ingroup  = between (char '{') (char '}') $ many groupParser
    garbage  = between (char '<') (char '>') $ many (TTerminal <$> noneOf ">")
    outgroup = noneOf "{}"

scoreNodes :: TGroup -> Int
scoreNodes = scoreNodes' 1
  where
    scoreNodes' :: Int -> TGroup -> Int
    scoreNodes' score tgroup = case tgroup of
      TNode xs -> score + sum ((fmap (scoreNodes' (score + 1))) xs)
      _ -> 0

sizeGarbage :: TGroup -> Int
sizeGarbage tgroup = case tgroup of
  TNode xs         -> sum (fmap sizeGarbage xs)
  TGarbage garbage -> length garbage
  _ -> 0

minput :: IO [String]
minput = fmap (lines) txtdata09

day09answer1 = do
  input <- minput
  return $ either (error . show) id . fmap scoreNodes . applyP groupParser . sanitize . head $ input

day09answer2 = do
  input <- minput
  return $ either (error. show) id . fmap sizeGarbage . applyP groupParser . sanitize . head $ input
