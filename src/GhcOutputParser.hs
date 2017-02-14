{-# LANGUAGE OverloadedStrings #-}

module GhcOutputParser (
  testParse
  ) where

import Text.Parsec
import Text.Parsec.Char

import Data.Maybe (fromMaybe)
import Control.Monad (void)

testParse = do
      input <- readFile "./test/testfile.txt"
      parseTest messages input
      -- return (runParser p () "testfile" input)

messages =
  (eof >> return [])
  <|> (do
    m1 <- try compileMessage
    m2 <- messages
    return $ m1 ++ m2
    )
  <|> (try skipRestOfLine >> messages)
  <|> (skipToEof >> messages)

compileMessage = do
  filepath <- many1 (noneOf "\n:") <?> "filepath"
  string ":"
  line <- location
  col <- location
  skipMany whitespace
  level <- string "err" <|> string "warn"
  skipRestOfLine
  msg <- many indentedLine
  return [(filepath, line, col, level, msg)]

location = do
  from <- many1 digit
  optional $ string "-"
  to <- optionMaybe (many1 digit)
  string ":"
  return (from, fromMaybe from to)

indentedLine = do
  indentation <- many1 whitespace <?> "indentation"
  content <- many1 (noneOf "\n") <?> "content"
  skipRestOfLine <|> eof
  return $ indentation ++ content

whitespace = oneOf "\t "
skipRestOfLine = void restOfLine
skipToEof = void $ manyTill anyChar eof
restOfLine = many (noneOf "\n") >> string "\n"
