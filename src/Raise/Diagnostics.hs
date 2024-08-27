{-# LANGUAGE FlexibleContexts #-}

module Raise.Diagnostics (
  diagnosticHandler
) where

import           Control.Lens                ((^.))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Either                 (fromRight)
import           Data.Maybe                  (fromJust)
import qualified Data.Text                   as T
import           Language.LSP.Diagnostics    (partitionBySource)
import qualified Language.LSP.Protocol.Lens  as J
import qualified Language.LSP.Protocol.Types as J
import           Language.LSP.Server
import           Raise.DiagnosticParser      (parseRSLTC)
import           System.Directory            (withCurrentDirectory)
import           System.FilePath             (takeDirectory, takeFileName)
import           System.Process              (readProcessWithExitCode)

runTool :: String -> [String] -> FilePath -> IO T.Text
runTool tool args path = do
  let dir = takeDirectory path
      file = takeFileName path
  withCurrentDirectory dir $ do
    (_, stdout, _) <- readProcessWithExitCode tool (args ++ [file]) ""
    pure $ T.pack stdout

runChecker :: FilePath -> IO T.Text
runChecker path = runTool "rsltc" [] path

runCompiler :: FilePath -> IO T.Text
runCompiler path = runTool "rsltc" ["-m"] path

sendDiagnostics :: Bool -> J.NormalizedUri -> FilePath -> LspM () ()
sendDiagnostics compile fileUri filePath = do
  tcDiags <- fromRight [] . parseRSLTC <$> liftIO (runChecker filePath)
  compilerDiags <- if not compile then pure [] else
    fmap (\x -> x {J._severity = Just J.DiagnosticSeverity_Warning} ) . fromRight [] . parseRSLTC <$> liftIO (runCompiler filePath)
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
