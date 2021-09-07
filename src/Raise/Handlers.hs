module Raise.Handlers (
  handlers
) where

import           Language.LSP.Server
import           Language.LSP.Types
import           Raise.CodeLens      (registerLenses)
import           Raise.Diagnostics   (diagnosticHandler)


handlers :: Bool -> Handlers (LspM ())
handlers compile = mconcat
  [ notificationHandler SInitialized $ const registerLenses
  , notificationHandler STextDocumentDidSave $ diagnosticHandler compile
  , notificationHandler STextDocumentDidOpen $ diagnosticHandler compile
  ]
