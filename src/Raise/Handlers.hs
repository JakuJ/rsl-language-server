module Raise.Handlers (
  handlers
) where

import           Language.LSP.Protocol.Message
import           Language.LSP.Server
import           Raise.CodeLens                (registerLenses)
import           Raise.Diagnostics             (diagnosticHandler)


handlers :: Bool -> Handlers (LspM ())
handlers compile = mconcat
  [ notificationHandler SMethod_Initialized $ const registerLenses
  , notificationHandler SMethod_TextDocumentDidSave $ diagnosticHandler compile
  , notificationHandler SMethod_TextDocumentDidOpen $ diagnosticHandler compile
  , notificationHandler SMethod_TextDocumentDidChange $ const $ pure ()
  , notificationHandler SMethod_TextDocumentDidClose $ const $ pure ()
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ const $ pure ()
  ]
