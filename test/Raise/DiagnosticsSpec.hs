module Raise.DiagnosticsSpec (
  spec
) where

import           Control.Lens            ((^.))
import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Language.LSP.Test       hiding (message)
import           Language.LSP.Types
import           Language.LSP.Types.Lens
import           Test.Hspec

sessionConfig :: SessionConfig
sessionConfig = defaultConfig
  { messageTimeout = 3
  }

anyTimeout :: SessionException -> Bool
anyTimeout (Timeout _) = True
anyTimeout _           = False

timesOut :: FilePath -> Expectation
timesOut file = session `shouldThrow` anyTimeout
  where
    session = runSessionWithConfig sessionConfig "rsl-language-server" fullCaps "test/data" $ do
      openDoc file "rsl"
      waitForDiagnosticsSource "rsl-language-server"

spec :: Spec
spec = parallel $ do
  describe "e2e tests" $ do
    it "does not report diagnostics when there are no errors" $ timesOut "no_errors.rsl"
    it "does not report compiler diagnostics when no flag is provided" $ timesOut "compiler_errors.rsl"
    it "report diagnostics when there are errors" $ runSessionWithConfig sessionConfig "rsl-language-server" fullCaps "test/data" $ do
      openDoc "two_errors.rsl" "rsl"
      [d1, d2] <- waitForDiagnosticsSource "rsl-language-server"
      liftIO $ do
        d1 ^. severity `shouldBe` Just DsError
        d1 ^. range `shouldBe` Range (Position 9 28) (Position 9 28)
        d1 ^. message `shouldBe` "Value name n2 hidden, renamed, or not defined"

        d2 ^. severity `shouldBe` Just DsError
        d2 ^. range `shouldBe` Range (Position 11 41) (Position 11 41)
        d2 ^. message `shouldBe` "Text and any-set and Int\ncannot be argument type for Person (i.e. Text) >< Database (i.e. Text-set) -> Database (i.e. Text-set)"
    it "report compiler diagnostics when flag is provided" $ runSessionWithConfig sessionConfig "rsl-language-server --compile" fullCaps "test/data" $ do
      openDoc "compiler_errors.rsl" "rsl"
      ds <- waitForDiagnosticsSource "rsl-language-server"
      liftIO $ do
        mapM_ (\d -> d ^. severity `shouldBe` Just DsWarning) ds
        let messages = [ "Feature not supported: abstract type\nType Resource-set seems to be involved in mutual recursion with another record or variant.\nFeature not supported: Mutual recursion between records or variants"
                        , "Internal error: no type alias for type Resource"
                        , "Internal error: no type alias for type Pool (i.e. Resource-set)"
                        , "Feature not supported: implicit function"
                        ]
        forM_ messages $ \m -> any (\d -> d ^. message == m) ds `shouldBe` True
