
import Test.Hspec
import qualified Data.Text as T

import qualified GhcOutputParserSpec

import GhcOutputParser

-- main :: IO ()
-- main = hspec $ do
--   GhcOutputParserSpec.spec

main = do
  putStrLn ""
  testParse
  putStrLn ""
