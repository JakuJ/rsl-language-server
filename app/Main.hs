{-# LANGUAGE RecordWildCards #-}

module Main (
  main
) where

import           ArgParse
import           Control.Monad.IO.Class      (liftIO)
import           Data.Version                (showVersion)
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import qualified Paths_rsl_language_server   as Paths
import           Raise.Handlers              (handlers)

syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose = Just True
  , _change = Just TextDocumentSyncKind_Incremental
  , _willSave = Just False
  , _willSaveWaitUntil = Just False
  , _save = Just $ InR $ SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { optTextDocumentSync = Just syncOptions
  }

serverDef :: Bool -> ServerDefinition ()
serverDef compile = ServerDefinition
            { parseConfig = const $ const $ Right ()
            , onConfigChange = const $ pure ()
            , defaultConfig = ()
            , configSection = "rsl-language-server"
            , doInitialize = \env _ -> pure $ Right env
            , staticHandlers = const $ handlers compile
            , interpretHandler = \env -> Iso (runLspT env) liftIO
            , options = lspOptions
            }

main :: IO Int
main = do
  Args{..} <- parseArgs
  if version then
    putStrLn ("rsl-language-server version " <> showVersion Paths.version) >> return 0
  else
    runServer $ serverDef compile
