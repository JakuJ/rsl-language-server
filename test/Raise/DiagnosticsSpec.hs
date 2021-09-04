module Raise.DiagnosticsSpec (
  spec
) where

import           Control.Lens            ((^.))
import           Control.Monad.IO.Class  (liftIO)
import           Language.LSP.Test       hiding (message)
import           Language.LSP.Types
import           Language.LSP.Types.Lens
import           Test.Hspec

sessionConfig :: SessionConfig
sessionConfig = defaultConfig
  { messageTimeout = 10
  }

spec :: Spec
spec = do
  describe "e2e tests" $ do
    it "report diagnostics when there are errors" $ runSessionWithConfig sessionConfig "rsl-language-server" fullCaps "test/data" $ do
      openDoc "two_errors.rsl" "rsl"
      [d1, d2] <- waitForDiagnosticsSource "rsl-language-server"
      liftIO $ do
        d1 ^. severity `shouldBe` Just DsError
        d1 ^. range `shouldBe` Range (Position 9 28) (Position 9 28)
        d1 ^. message `shouldBe` "Value name n2 hidden, renamed, or not defined"

        d2 ^. severity `shouldBe` Just DsError
        d2 ^. range `shouldBe` Range (Position 11 41) (Position 11 41)
        d2 ^. message `shouldBe` "Text and any-set and Int cannot be argument type for Person (i.e. Text) >< Database (i.e. Text-set) -> Database (i.e. Text-set)"
