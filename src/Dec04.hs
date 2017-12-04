{-# LANGUAGE OverloadedStrings #-}

module Dec04 where

import Data.List

day04file :: String
day04file = "src/day04input1.txt"

txtdata :: IO String
txtdata = readFile day04file

textlist :: IO [[String]]
textlist = fmap (fmap words . lines) txtdata

-- nodupline checks if we've removed any duplicates
nodupline :: [String] -> Bool
nodupline line = (length line) == (length (nub line))

-- @length . filter id@ gets the number of True in a list
day04answer1 = fmap (length . filter id . fmap nodupline) textlist
-- @fmap (fmap sort)@ lexographically sorts each word, anagrams will become equal
day04answer2 = fmap (length . filter id . fmap nodupline . fmap (fmap sort)) textlist
