module Raise.Handlers (
    handlers
) where

import           Language.LSP.Server
import           Language.LSP.Types
import           Raise.CodeLens

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ const registerLenses
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "raise-language-server" "Hover not implemented yet"
          range = Range pos pos
      responder (Right $ Just rsp)
  ]
