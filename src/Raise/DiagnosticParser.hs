module Raise.DiagnosticParser where

import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Language.LSP.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (skipLineComment)

type Parser = Parsec Void T.Text

mkDiagnostic :: Int -> Int -> T.Text -> Diagnostic
mkDiagnostic line column msg = Diagnostic
    { _range = Range (Position line column) (Position line column)
    , _severity = Just DsError
    , _code = Nothing
    , _source = Just "rsl-language-server"
    , _message = msg
    , _tags = Nothing
    , _relatedInformation = Just $ List []
    }

parseHeader :: Parser ()
parseHeader = skipLineComment "rsltc version" >> void newline

parseCheckStart :: Parser ()
parseCheckStart = skipLineComment "Checking" >> void newline

parseCheckEnd :: Parser ()
parseCheckEnd = skipLineComment "Finished" >> void newline

parseDiagnosticLine :: Parser T.Text
parseDiagnosticLine = T.pack <$> someTill asciiChar newline

parseDiagnostic :: Parser Diagnostic
parseDiagnostic = do
    lookAhead $ oneOf ("./" :: String) -- filepaths start with . (relative) or / (absolute)
    someTill asciiChar (string ".rsl:")
    line <- read <$> someTill digitChar (char ':')
    column <- read <$> someTill digitChar (char ':')
    hspace
    message <- parseDiagnosticLine
    extraLines <- many extraDignosticLine
    let fullMessage = T.unwords $ message : extraLines
    pure $ mkDiagnostic (line - 1) column fullMessage

extraDignosticLine :: Parser T.Text
extraDignosticLine = do
    notFollowedBy (parseCheckStart <|> parseCheckEnd <|> void parseSummary <|> void parseDiagnostic)
    parseDiagnosticLine

parseSummary :: Parser (Int, Int)
parseSummary = do
    string "rsltc completed: "
    errs <- read <$> some numberChar
    string " error(s) "
    warns <- read <$> some numberChar
    string " warning(s)"
    optional newline
    pure (errs, warns)

parseLine :: Parser [Diagnostic]
parseLine = ((: []) <$> try parseDiagnostic) <|> ((parseCheckStart <|> parseCheckEnd) >> pure [])

parseCorrectSyntax :: Parser [Diagnostic]
parseCorrectSyntax = between parseHeader parseSummary $ concat <$> many parseLine

parseSyntaxError :: Parser [Diagnostic]
parseSyntaxError = (: []) <$> parseDiagnostic

parseRSLTC :: T.Text -> Either (ParseErrorBundle T.Text Void) [Diagnostic]
parseRSLTC = runParser (parseCorrectSyntax <|> parseSyntaxError) ""
