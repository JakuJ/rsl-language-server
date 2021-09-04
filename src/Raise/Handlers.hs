module Raise.Handlers (
    handlers
) where

import           Language.LSP.Server
import           Language.LSP.Types
import           Raise.CodeLens      (registerLenses)
import           Raise.Diagnostics   (diagnosticHandler)


handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ const registerLenses
  , notificationHandler STextDocumentDidSave diagnosticHandler
  , notificationHandler STextDocumentDidOpen diagnosticHandler
  ]
