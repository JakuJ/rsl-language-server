{-# LANGUAGE RecordWildCards #-}

module Raise.DiagnosticParserSpec (
    spec
) where

import           Data.Either            (isRight)
import qualified Data.Text              as T
import           Data.Void              (Void)
import           Language.LSP.Types
import           Raise.DiagnosticParser
import           Test.Hspec
import           Text.Megaparsec        (ParseErrorBundle, runParser)

run :: Parser a -> String -> Either (ParseErrorBundle String Void) a
run p = runParser p ""

spec :: Spec
spec = do
  describe "Raise.DiagnosticParser - unit" $ do
    it "should parse the header" $ do
      run parseHeader "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014\n" `shouldSatisfy` isRight
    it "should parse the 'Checking' message" $ do
      run parseCheckStart "Checking SET_DATABASE ... \n" `shouldSatisfy` isRight
    it "should parse the 'Finished' message" $ do
      run parseCheckEnd "Finished SET_DATABASE\n" `shouldSatisfy` isRight
    it "should parse summary" $ do
      run parseSummary "rsltc completed: 1 error(s) 0 warning(s)" `shouldBe` Right (1, 0)
    it "should parse diagnostics" $ do
      let Right Diagnostic {..} = run parseDiagnostic "./SET_DATABASE.rsl:12:7: Structure of product binding (n, m) does not match structure of type Nat\n"
      _range `shouldBe` Range (Position 11 7) (Position 11 7)
      _message `shouldBe` "Structure of product binding (n, m) does not match structure of type Nat"
      _source `shouldBe` Just "SET_DATABASE.rsl"
  describe "Raise.DiagnosticParser - integration" $ do
    it "should parse example output" $ do
      let example = unlines [ "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014"
                            , "Checking SET_DATABASE ... "
                            , "./SET_DATABASE.rsl:12:45: Value name n1 hidden, renamed, or not defined"
                            , "Finished SET_DATABASE"
                            , "rsltc completed: 1 error(s) 0 warning(s)" ]
          result = parseRSLTC $ T.pack example
          Right [Diagnostic {..}] = result
      result `shouldSatisfy` isRight
      _range `shouldBe` Range (Position 11 45) (Position 11 45)
      _message `shouldBe` "Value name n1 hidden, renamed, or not defined"
      _source `shouldBe` Just "SET_DATABASE.rsl"
