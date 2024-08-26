module Raise.CodeLens (
    registerLenses
) where

import           Control.Monad       (void)
import qualified Data.Text           as T
import           Language.LSP.Server
import           Language.LSP.Types

regOpts :: CodeLensRegistrationOptions
regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)

registerCommand :: T.Text -> T.Text -> CodeLens
registerCommand name command = CodeLens
  { _range = mkRange 0 0 0 100
  , _command = Just $ Command name command Nothing
  , _xdata = Nothing
  }

cmdList :: [(T.Text, T.Text)]
cmdList = [ ("Typecheck", "raise.typeCheck")
          , ("Compile to SML", "raise.compileToSML")
          , ("Run SML", "raise.runSML")
          , ("Save Results", "raise.saveResults")
          ]

registerLenses :: LspM () ()
registerLenses = do
  let rsp = map (uncurry registerCommand) cmdList
  void $ registerCapability STextDocumentCodeLens regOpts $ \_ responder -> responder (Right (List rsp))
