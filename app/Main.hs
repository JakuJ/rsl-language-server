module Main (
  main
) where

import           Control.Monad.IO.Class (liftIO)
import           Language.LSP.Server
import           Language.LSP.Types
import           Raise.Handlers         (handlers)

syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose = Just True
  , _change = Just TdSyncIncremental
  , _willSave = Just False
  , _willSaveWaitUntil = Just False
  , _save = Just $ InR $ SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { textDocumentSync = Just syncOptions
  , executeCommandCommands = Just []
  }

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _ -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = lspOptions
  , defaultConfig = undefined
  }
