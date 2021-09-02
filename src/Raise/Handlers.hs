module Raise.Handlers (
    handlers
) where

import           Language.LSP.Server
import           Language.LSP.Types
import           Raise.CodeLens
import           Raise.Diagnostics


handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ const registerLenses
  , notificationHandler STextDocumentDidSave onSaveHandler
  ]
