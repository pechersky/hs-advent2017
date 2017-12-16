module AOCommon where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.List

type Parser = Parsec Void String

parseInput :: Parser a -> String -> IO a
parseInput p input = do
  case parse p "input" input of
    Left e  -> fail (parseErrorTextPretty e)
    Right a -> return a

parseLines :: String -> Parser a -> IO [a]
parseLines filepath p = do
  input <- readFile filepath
  parseInput (many (p <* optional newline) <* optional eof) input
