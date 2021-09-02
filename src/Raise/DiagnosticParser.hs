module Raise.DiagnosticParser where

import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Language.LSP.Types
import           Language.LSP.Types.Lens    (HasCharacter (character))
import           System.FilePath            (takeFileName)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (skipLineComment)


type Parser = Parsec Void String

mkDiagnostic :: String -> Int -> Int -> String -> Diagnostic
mkDiagnostic source line column msg = Diagnostic
                                (Range (Position line column) (Position line column))
                                (Just DsError)  -- severity
                                Nothing  -- code
                                (Just (T.pack source)) -- source
                                (T.pack msg)
                                Nothing -- tags
                                (Just (List []))

parseIdentifier :: Parser String
parseIdentifier = some alphaNumChar

parseHeader :: Parser ()
parseHeader = do
    skipLineComment "rsltc version"
    void newline

parseCheckStart :: Parser ()
parseCheckStart = skipLineComment "Checking" >> void newline

parseCheckEnd :: Parser ()
parseCheckEnd = skipLineComment "Finished" >> void newline

parseDiagnostic :: Parser Diagnostic
parseDiagnostic = do
    path <- some $ anySingleBut ':'
    char ':'
    line <- some $ anySingleBut ':'
    char ':'
    column <- some $ anySingleBut ':'
    char ':'
    hspace
    message <- some $ anySingleBut '\n'
    newline
    let lineNo = read line :: Int
        columnNo = read column :: Int
    return $ mkDiagnostic (takeFileName path) (lineNo - 1) columnNo message

parseSummary :: Parser (Int, Int)
parseSummary = do
    string "rsltc completed: "
    errs <- read <$> some numberChar
    string " error(s) "
    warns <- read <$> some numberChar
    string " warning(s)"
    optional newline
    return (errs, warns)

parseOutput :: Parser [Diagnostic]
parseOutput = do
    parseHeader
    parseCheckStart
    diags <- many $ try parseDiagnostic
    parseCheckEnd
    (errs, warns) <- parseSummary
    return diags

parseRSLTC :: T.Text -> Either (ParseErrorBundle String Void) [Diagnostic]
parseRSLTC = runParser parseOutput "" . T.unpack
