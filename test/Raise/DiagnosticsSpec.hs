module Raise.DiagnosticsSpec (
  spec
) where

import           Control.Lens            ((^.))
import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Language.LSP.Test
import           Language.LSP.Types
import           Language.LSP.Types.Lens
import           Test.Hspec

spec :: Spec
spec = do
  describe "e2e tests" $ do
    -- TODO: Test for clearing diagnostics when there are no errors
    it "report diagnostics when there are errors" $ runSession "rsl-language-server" fullCaps "test/data" $ do
      openDoc "two_errors.rsl" "rsl"
      diags <- waitForDiagnosticsSource "rsl-language-server"
      liftIO $ forM_ diags $ \diag -> do
        diag ^. severity `shouldBe` Just DsError
        diag ^. source `shouldBe` Just "rsl-language-server"
