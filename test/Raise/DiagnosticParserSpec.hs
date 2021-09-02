{-# LANGUAGE RecordWildCards #-}

module Raise.DiagnosticParserSpec (
    spec
) where

import           Control.Lens            ((^.))
import           Data.Either             (isRight)
import qualified Data.Text               as T
import           Data.Void               (Void)
import           Language.LSP.Types
import           Language.LSP.Types.Lens (message, range)
import           Raise.DiagnosticParser
import           Test.Hspec
import           Text.Megaparsec

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
      let output = T.unlines [ "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014"
                            , "Checking SET_DATABASE ... "
                            , "Finished SET_DATABASE"
                            , "rsltc completed: 0 error(s) 0 warning(s)" ]
          result = parseRSLTC output
      result `shouldBe` Right []
    it "parse syntax error" $ do
      let output = "./SET_DATABASE.rsl:9:29: syntax error\n"
          result = parseRSLTC output
          Right [Diagnostic {..}] = result
      result `shouldSatisfy` isRight
      _range `shouldBe` Range (Position 8 29) (Position 8 29)
      _message `shouldBe` "syntax error"
    it "parse output with a one-line error" $ do
      let output = T.unlines [ "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014"
                            , "Checking SET_DATABASE ... "
                            , "./SET_DATABASE.rsl:12:45: Value name n1 hidden, renamed, or not defined"
                            , "Finished SET_DATABASE"
                            , "rsltc completed: 1 error(s) 0 warning(s)" ]
          result = parseRSLTC output
          Right [Diagnostic {..}] = result
      result `shouldSatisfy` isRight
      _range `shouldBe` Range (Position 11 45) (Position 11 45)
      _message `shouldBe` "Value name n1 hidden, renamed, or not defined"
    it "parses mixed diagnostics and checking/finished messages" $ do
      let output = T.unlines [ "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014"
                            , "./SET_DATABASE.rsl:1:1: Module name SET_DATA does not match file name SET_DATABASE.rsl"
                            , "Checking SET_DATABASE ... "
                            , "Finished SET_DATABASE"
                            , "rsltc completed: 1 error(s) 0 warning(s)" ]
          result = parseRSLTC output
          Right [Diagnostic {..}] = result
      result `shouldSatisfy` isRight
      _range `shouldBe` Range (Position 0 1) (Position 0 1)
      _message `shouldBe` "Module name SET_DATA does not match file name SET_DATABASE.rsl"
    it "parses multiline diagnostics" $ do
      let output = T.unlines [ "rsltc version 2.6 of Fri Sep 19 19:41:13 BST 2014"
                            , "Checking SET_DATABASE ... "
                            , "./SET_DATABASE.rsl:7:25: Value name db1 hidden, renamed, or not defined"
                            , "./SET_DATABASE.rsl:7:40: Type Person (i.e. Text)"
                            , "and type Int"
                            , "are not compatible"
                            , "./SET_DATABASE.rsl:12:26: Value name n3 hidden, renamed, or not defined"
                            , "Finished SET_DATABASE"
                            , "rsltc completed: 1 error(s) 0 warning(s)" ]
          result = parseRSLTC output
          Right [d1, d2, d3] = result
      result `shouldSatisfy` isRight

      d1 ^. range `shouldBe` Range (Position 6 25) (Position 6 25)
      d1 ^. message `shouldBe` "Value name db1 hidden, renamed, or not defined"

      d2 ^. range `shouldBe` Range (Position 6 40) (Position 6 40)
      d2 ^. message `shouldBe` "Type Person (i.e. Text) and type Int are not compatible"

      d3 ^. range `shouldBe` Range (Position 11 26) (Position 11 26)
      d3 ^. message `shouldBe` "Value name n3 hidden, renamed, or not defined"
