{-# LANGUAGE OverloadedStrings #-}

module Dec04 where

import Data.List

day04file :: String
day04file = "src/day04input1.txt"

txtdata04 :: IO String
txtdata04 = readFile day04file

textlist04 :: IO [[String]]
textlist04 = fmap (fmap words . lines) txtdata04

-- nodupline checks if we've removed any duplicates
nodupline :: [String] -> Bool
nodupline line = (length line) == (length (nub line))

-- @length . filter id@ gets the number of True in a list
day04answer1 = fmap (length . filter id . fmap nodupline) textlist04
-- @fmap (fmap sort)@ lexographically sorts each word, anagrams will become equal
day04answer2 = fmap (length . filter id . fmap nodupline . fmap (fmap sort)) textlist04
