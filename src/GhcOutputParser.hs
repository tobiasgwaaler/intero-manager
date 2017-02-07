{-# LANGUAGE OverloadedStrings #-}

module GhcOutputParser (
  testParse, -- temp
  parseErrWarn,
  GhcErrWarn(..),
  Level(..)
  ) where

import Data.Text hiding (unlines, isPrefixOf, intercalate)
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (maybeToList)

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Text

import Data.Monoid ((<>))

data Level = ErrorLevel
           | WarningLevel
           deriving (Show, Eq)

mkLevel level
  | "err" `isPrefixOf` level = ErrorLevel
  | otherwise                = WarningLevel

data GhcErrWarn = GhcErrWarn {
  filepath :: FilePath,
  fromLine :: Int,
  fromOffset :: Int,
  toLine :: Int,
  toOffset :: Int,
  level :: Level,
  message :: String
} deriving (Show, Eq)

parseErrWarn :: Text -> Either ParseError [GhcErrWarn]
parseErrWarn = runParser errWarnParser () "parse GHC output"

errWarnParser = many errWarnMessage

errWarnMessage = do
  filepath'   <- many1 (noneOf ":") <?> "filepath"
  string ":" <?> "1"
  fromLine'   <- many1 (noneOf ":") <?> "fromLine"
  string ":" <?> "2"
  fromOffset' <- many1 (noneOf ":") <?> "fromOffset"
  string ":" <?> "3 "
  skipMany space
  level' <- string "warning" <?> "level"
  manyTill anyChar endOfLine
  message' <- many1 indentedLine <?> "message"

  return GhcErrWarn {
      filepath = filepath',
      fromLine = read fromLine',
      fromOffset = read fromOffset',
      toLine = read fromLine',
      toOffset = read fromOffset',
      level = mkLevel level',
      message = intercalate "\n" message'
    }

indentedLine = do
  indentation <- many1 (oneOf " \t")
  content <- many1 (noneOf "\n")
  return $ indentation <> content


testParse = parseTest p (
  "src/Lib.hs:17:1: warning: [-Wunused-imports]\n" <>
  "    fst\n" <>
  "   snd\n" <>
  "\n" <>
  "src/Foo.hs:90:33: warning: [-Wunused-imports]\n" <>
  "    foomsg1\n"
  :: Text)

p = many m

m = do
  filepath' <- many1 (noneOf ":") <?> "filepath"
  string ":"
  fromLine' <- many1 digit <?> "fromLine"
  string ":"
  fromOffset' <- many1 digit <?> "fromOffset"
  string ":"
  skipMany (string " ")
  level' <- many1 letter <?> "level"
  string ":"
  many (noneOf "\n")
  endOfLine
  msg' <- many1 indLine
  return (filepath', fromLine', fromOffset', level', msg')

indLine = do
  spaces <- many1 $ oneOf " \t"
  content <- many1 (noneOf "\n")
  eol <- string "\n"
  return $ spaces <> content <> eol
