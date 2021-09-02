{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (
  main
) where

import           Control.Monad.IO.Class
import           Language.LSP.Server
import           Language.LSP.Types
import           Raise.Handlers

-- TODO: Record fields issue
syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  (Just True)
  (Just TdSyncIncremental)
  (Just False)
  (Just False)
  (Just $ InR $ SaveOptions $ Just False)

lspOptions :: Options
lspOptions = defaultOptions
  { textDocumentSync = Just syncOptions
  , executeCommandCommands = Just []
  }

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = lspOptions
  }
