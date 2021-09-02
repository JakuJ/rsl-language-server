{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Raise.Diagnostics (
    onSaveHandler,
    onOpenHandler
) where

import           Control.Lens             ((^.))
import           Control.Monad.IO.Class   (liftIO)
import           Data.Maybe               (fromJust)
import           Data.Either               
import qualified Data.Text                as T
import           Language.LSP.Diagnostics
import           Language.LSP.Server
import qualified Language.LSP.Types       as J
import qualified Language.LSP.Types.Lens  as J
import           Raise.DiagnosticParser   (parseRSLTC)
import           System.Process           (readProcessWithExitCode)

runChecker :: FilePath -> IO T.Text
runChecker path = do
    (_, stdout, _) <- readProcessWithExitCode "raise.sh" ["rsltc", path] ""
    return $ T.pack stdout

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.NormalizedUri -> FilePath -> LspM () ()
sendDiagnostics fileUri filePath = do
  msgs <- liftIO $ runChecker filePath
  let diags = fromRight [] (parseRSLTC msgs)
  case diags of
    [] -> flushDiagnosticsBySource 100 (Just "rsl-language-server")
    _ -> publishDiagnostics 100 fileUri Nothing (partitionBySource diags)

onSaveHandler :: J.NotificationMessage 'J.TextDocumentDidSave -> LspM () ()
onSaveHandler msg = do
    let doc = msg ^. J.params . J.textDocument . J.uri
        fileName = J.uriToFilePath doc
    sendDiagnostics (J.toNormalizedUri doc) $ fromJust fileName

-- TODO: Unify type signatures
onOpenHandler :: J.NotificationMessage 'J.TextDocumentDidOpen -> LspM () ()
onOpenHandler msg = do
    let doc = msg ^. J.params . J.textDocument . J.uri
        fileName = J.uriToFilePath doc
    sendDiagnostics (J.toNormalizedUri doc) $ fromJust fileName
