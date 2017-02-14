{-# LANGUAGE OverloadedStrings #-}

module GhcOutputParser (
  testParse
  ) where

import Text.Parsec
import Text.Parsec.Char

import Control.Monad (void)

testParse = do
      input <- readFile "./test/testfile.txt"
      parseTest p input
      -- return (runParser p () "testfile" input)

p = messages

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
  linenum <- many1 digit
  string ":"
  colnum <- many1 digit
  string ":"
  skipMany whitespace
  level <- string "err" <|> string "warn"
  skipRestOfLine
  msg <- many indentedLine
  return [(filepath, linenum, colnum, level, msg)]

indentedLine = do
  indentation <- many1 whitespace <?> "indentation"
  content <- many1 (noneOf "\n") <?> "content"
  skipRestOfLine <|> eof
  return $ indentation ++ content

whitespace = oneOf "\t "
skipRestOfLine = void restOfLine
skipToEof = void $ manyTill anyChar eof
restOfLine = many (noneOf "\n") >> string "\n"
