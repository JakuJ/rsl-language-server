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

runTool :: String -> [String] -> IO T.Text
runTool tool args = do
  (_, stdout, _) <- readProcessWithExitCode tool args ""
  pure $ T.pack stdout

runChecker :: FilePath -> IO T.Text
runChecker path = runTool "rsltc" [path]

runCompiler :: FilePath -> IO T.Text
runCompiler path = runTool "rsltc" ["-m", path]

sendDiagnostics :: Bool -> J.NormalizedUri -> FilePath -> LspM () ()
sendDiagnostics compile fileUri filePath = do
  tcDiags <- fromRight [] . parseRSLTC <$> liftIO (runChecker filePath)
  compilerDiags <- if not compile then pure [] else 
    fmap (\x -> x {J._severity = Just J.DsWarning} ) . fromRight [] . parseRSLTC <$> liftIO (runCompiler filePath)
  let diags = tcDiags ++ compilerDiags
  if null diags then
    flushDiagnosticsBySource 100 (Just "rsl-language-server")
  else
    publishDiagnostics 100 fileUri Nothing (partitionBySource diags)

diagnosticHandler :: (J.HasParams msg params, J.HasTextDocument params doc, J.HasUri doc J.Uri) => Bool -> msg -> LspM () ()
diagnosticHandler compile msg = do
  let doc = msg ^. J.params . J.textDocument . J.uri
      fileName = J.uriToFilePath doc
  sendDiagnostics compile (J.toNormalizedUri doc) $ fromJust fileName
