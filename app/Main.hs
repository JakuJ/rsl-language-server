module Main (
  main
) where

import           ArgParse               (parseArgs)
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

serverDef :: Bool -> ServerDefinition ()
serverDef compile = ServerDefinition
            { onConfigurationChange = const $ pure $ Right ()
            , doInitialize = \env _ -> pure $ Right env
            , staticHandlers = handlers compile
            , interpretHandler = \env -> Iso (runLspT env) liftIO
            , options = lspOptions
            , defaultConfig = undefined
            }

main :: IO Int
main = do
  compile <- parseArgs
  runServer $ serverDef compile
