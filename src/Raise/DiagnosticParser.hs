module Raise.DiagnosticParser where

import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Language.LSP.Types
import           Language.LSP.Types.Lens    (HasCharacter (character))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (skipLineComment)


type Parser = Parsec Void String

mkDiagnostic :: Int -> Int -> String -> Diagnostic
mkDiagnostic line column msg = Diagnostic
                                (Range (Position line column) (Position line column))
                                (Just DsError)  -- severity
                                Nothing  -- code
                                (Just "rsl-language-server") -- source
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

parseDiagnostic :: Parser [Diagnostic]
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
    return [mkDiagnostic (lineNo - 1) columnNo message]

parseSummary :: Parser (Int, Int)
parseSummary = do
    string "rsltc completed: "
    errs <- read <$> some numberChar
    string " error(s) "
    warns <- read <$> some numberChar
    string " warning(s)"
    optional newline
    return (errs, warns)

parseLine :: Parser [Diagnostic]
parseLine = try parseDiagnostic <|> ((parseCheckStart <|> parseCheckEnd) >> pure [])

parseSyntaxCorrect :: Parser [Diagnostic]
parseSyntaxCorrect = do
    parseHeader
    diags <- concat <$> many parseLine
    (errs, warns) <- parseSummary
    return diags

parseRSLTC :: T.Text -> Either (ParseErrorBundle String Void) [Diagnostic]
parseRSLTC = runParser (parseSyntaxCorrect <|> parseDiagnostic) "" . T.unpack
