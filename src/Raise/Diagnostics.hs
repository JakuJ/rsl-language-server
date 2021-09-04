{-# LANGUAGE FlexibleContexts #-}

module Raise.Diagnostics (
    diagnosticHandler
) where

import           Control.Lens             ((^.))
import           Control.Monad.IO.Class   (liftIO)
import           Data.Either              (fromRight)
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import           Language.LSP.Diagnostics (partitionBySource)
import           Language.LSP.Server
import qualified Language.LSP.Types       as J
import qualified Language.LSP.Types.Lens  as J
import           Raise.DiagnosticParser   (parseRSLTC)
import           System.Process           (readProcessWithExitCode)

runChecker :: FilePath -> IO T.Text
runChecker path = do
    (_, stdout, _) <- readProcessWithExitCode "rsltc" [path] ""
    pure $ T.pack stdout

sendDiagnostics :: J.NormalizedUri -> FilePath -> LspM () ()
sendDiagnostics fileUri filePath = do
  diagnostics <- fromRight [] . parseRSLTC <$> liftIO (runChecker filePath)
  if null diagnostics then
    flushDiagnosticsBySource 100 (Just "rsl-language-server")
  else
    publishDiagnostics 100 fileUri Nothing (partitionBySource diagnostics)

diagnosticHandler :: (J.HasParams msg params, J.HasTextDocument params doc, J.HasUri doc J.Uri) => msg -> LspM () ()
diagnosticHandler msg = do
    let doc = msg ^. J.params . J.textDocument . J.uri
        fileName = J.uriToFilePath doc
    sendDiagnostics (J.toNormalizedUri doc) $ fromJust fileName
