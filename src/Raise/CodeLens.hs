module Raise.CodeLens (
    registerLenses
) where

import           Control.Monad
import qualified Data.Text           as T
import           Language.LSP.Server
import           Language.LSP.Types

regOpts :: CodeLensRegistrationOptions
regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)

registerCommand :: T.Text -> T.Text -> CodeLens
registerCommand name command = CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing
    where
        cmd = Command name command Nothing

cmdList :: [(T.Text, T.Text)]
cmdList =   [("Typecheck", "raise.typeCheck")
            ,("Compile to SML", "raise.compileToSML")
            ,("Run SML", "raise.runSML")]

registerLenses :: LspM () ()
registerLenses = do
    let rsp = uncurry registerCommand <$> cmdList
    void $ registerCapability STextDocumentCodeLens regOpts $ \_req responder -> responder (Right (List rsp))
