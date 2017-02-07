{-# LANGUAGE OverloadedStrings #-}

module GhcOutputParserSpec where

import Test.Hspec

import GhcOutputParser
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (intercalate)

spec = describe "GhcOutputParser" $ do
    it "parses single message" $
      parse
        [ "src/Lib.hs:17:1: warning: [-Wunused-imports]"
        , "    The import of ‘Data.Char’ is redundant"
        , "      except perhaps to import instances from ‘Data.Char’"
        , "    To import instances alone, use: import Data.Char()"
        ]
      `shouldBe`
        Right [GhcErrWarn {
          filepath = "src/Lib.hs",
          fromLine = 17,
          fromOffset = 1,
          toLine = 17,
          toOffset = 1,
          level = WarningLevel,
          message = joinStr
          [ "    The import of ‘Data.Char’ is redundant"
          , "      except perhaps to import instances from ‘Data.Char’"
          , "    To import instances alone, use: import Data.Char()"
          ]
        }]

    it "parses multiple messages" $
      parse
        [ "src/Lib.hs:17:1: warning: [-Wunused-imports]"
        , "    The import of ‘Data.Char’ is redundant"
        , ""
        , "src/Lib.hs:17:1: warning: [-Wunused-imports]"
        , "    The import of ‘Data.Char’ is redundant"
        ]
      `shouldBe`
        Right [GhcErrWarn {
          filepath = "src/Lib.hs",
          fromLine = 17,
          fromOffset = 1,
          toLine = 17,
          toOffset = 1,
          level = WarningLevel,
          message = "    The import of ‘Data.Char’ is redundant"
        }, GhcErrWarn {
          filepath = "src/Lib.hs",
          fromLine = 17,
          fromOffset = 1,
          toLine = 17,
          toOffset = 1,
          level = WarningLevel,
          message = "    The import of ‘Data.Char’ is redundant"
        }
        ]

-- helpers
parse = parseErrWarn . joinText
joinText = T.intercalate "\n"
joinStr = intercalate "\n"
