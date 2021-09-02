{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Raise.Diagnostics (
    onSaveHandler
) where

import           Control.Lens
import           Control.Monad.IO.Class   (liftIO)
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import           Language.LSP.Diagnostics
import           Language.LSP.Server
import qualified Language.LSP.Types       as J
import qualified Language.LSP.Types.Lens  as J
import           Raise.DiagnosticParser   (parseRSLTC)
import           System.Process

runChecker :: FilePath -> IO T.Text
runChecker path = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "raise.sh" ["rsltc", path] ""
    return $ T.pack stdout

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.NormalizedUri -> FilePath -> LspM () ()
sendDiagnostics fileUri filePath = do
  msgs <- liftIO $ runChecker filePath
  case parseRSLTC msgs of
      Left _ -> publishDiagnostics 100 fileUri Nothing (partitionBySource [])
      Right ds -> publishDiagnostics 100 fileUri Nothing (partitionBySource ds)

onSaveHandler :: J.NotificationMessage 'J.TextDocumentDidSave -> LspM () ()
onSaveHandler msg = do
    let doc = msg ^. J.params . J.textDocument . J.uri
        fileName = J.uriToFilePath doc
    sendDiagnostics (J.toNormalizedUri doc) $ fromJust fileName
