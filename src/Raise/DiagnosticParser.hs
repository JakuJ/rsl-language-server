{-# LANGUAGE TypeApplications #-}

module Raise.DiagnosticParser (
    parseRSLTC
) where

import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Language.LSP.Types
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
    lookAhead $ oneOf @[] "./"
    someTill asciiChar (string ".rsl:")
    line <- someTill digitChar (char ':')
    column <- someTill digitChar (char ':')
    hspace
    message <- someTill asciiChar newline
    other_lines <- many adline
    let lineNo = read line :: Int
        columnNo = read column :: Int
        full_msg = unwords $ message : other_lines
    return [mkDiagnostic (lineNo - 1) columnNo full_msg]

adline :: Parser String
adline = do
    notFollowedBy (parseCheckStart <|> parseCheckEnd <|> void parseSummary <|> void parseDiagnostic)
    someTill asciiChar newline

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
    parseSummary
    return diags

parseRSLTC :: T.Text -> Either (ParseErrorBundle String Void) [Diagnostic]
parseRSLTC = runParser (parseSyntaxCorrect <|> parseDiagnostic) "" . T.unpack
