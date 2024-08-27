module Raise.CodeLens (
    registerLenses
) where

import           Control.Monad.IO.Class
import qualified Data.Text                     as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server

regOpts :: CodeLensRegistrationOptions
regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)

registerCommand :: T.Text -> T.Text -> CodeLens
registerCommand name command = CodeLens
  { _range = mkRange 0 0 0 100
  , _command = Just $ Command name command Nothing
  , _data_ = Nothing
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
  _ <- registerCapability SMethod_TextDocumentCodeLens regOpts $ \_ responder -> do
    responder (Right (InL rsp))
  pure ()
