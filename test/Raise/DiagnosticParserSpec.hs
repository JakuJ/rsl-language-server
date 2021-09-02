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
  describe "unit tests" $ do
    it "parse the header" $ do
      run parseHeader "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014\n" `shouldSatisfy` isRight
    it "parse the 'Checking' message" $ do
      run parseCheckStart "Checking SET_DATABASE ... \n" `shouldSatisfy` isRight
    it "parse the 'Finished' message" $ do
      run parseCheckEnd "Finished SET_DATABASE\n" `shouldSatisfy` isRight
    it "parse error summary" $ do
      run parseSummary "rsltc completed: 1 error(s) 0 warning(s)" `shouldBe` Right (1, 0)
    it "parse diagnostics" $ do
      let Right [Diagnostic {..}] = run parseDiagnostic "./SET_DATABASE.rsl:12:7: Structure of product binding (n, m) does not match structure of type Nat\n"
      _range `shouldBe` Range (Position 11 7) (Position 11 7)
      _message `shouldBe` "Structure of product binding (n, m) does not match structure of type Nat"
  describe "integration tests" $ do
    it "parse output with no errors" $ do
      let example = T.unlines [ "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014"
                            , "Checking SET_DATABASE ... "
                            , "Finished SET_DATABASE"
                            , "rsltc completed: 0 error(s) 0 warning(s)" ]
          result = parseRSLTC example
      result `shouldBe` Right []
    it "parse output with a one-line error" $ do
      let example = T.unlines [ "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014"
                            , "Checking SET_DATABASE ... "
                            , "./SET_DATABASE.rsl:12:45: Value name n1 hidden, renamed, or not defined"
                            , "Finished SET_DATABASE"
                            , "rsltc completed: 1 error(s) 0 warning(s)" ]
          result = parseRSLTC example
          Right [Diagnostic {..}] = result
      result `shouldSatisfy` isRight
      _range `shouldBe` Range (Position 11 45) (Position 11 45)
      _message `shouldBe` "Value name n1 hidden, renamed, or not defined"
    it "parses mixed diagnostics and checking/finished messages" $ do
      let example = T.unlines [ "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014"
                            , "./SET_DATABASE.rsl:1:1: Module name SET_DATA does not match file name SET_DATABASE.rsl"
                            , "Checking SET_DATABASE ... "
                            , "Finished SET_DATABASE"
                            , "rsltc completed: 1 error(s) 0 warning(s)" ]
          result = parseRSLTC example
          Right [Diagnostic {..}] = result
      result `shouldSatisfy` isRight
      _range `shouldBe` Range (Position 0 1) (Position 0 1)
      _message `shouldBe` "Module name SET_DATA does not match file name SET_DATABASE.rsl"
