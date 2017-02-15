{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DeriveGeneric #-}

module GhcOutputParser (
  parseCompileErrors
  ) where

import Text.Parsec
import Text.Parsec.Char

import Data.Text (Text)

import Data.Maybe (fromMaybe)
import Control.Monad (void)

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

-- testParse = do
--       input <- readFile "./test/testfile.txt"
--       parseTest messages input

parseCompileErrors :: Text -> Either ParseError [CompileError]
parseCompileErrors = runParser messages () ""

data CompileErrors = CompileErrors {
    compileErrors :: [CompileError]
  } deriving (Generic, Show)

data CompileError = CompileError {
  filepath :: String,
  line :: (String, String),
  col :: (String, String),
  level :: String,
  msg :: [String]
} deriving (Generic, Show)

instance ToJSON CompileError
instance ToJSON CompileErrors

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
  return [CompileError {filepath, line , col, level, msg}]

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
