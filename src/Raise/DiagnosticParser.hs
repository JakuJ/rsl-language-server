module Raise.DiagnosticParser where

import           Control.Monad               (void)
import           Data.Foldable               (asum)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Data.Void                   (Void)
import           Language.LSP.Protocol.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer  (skipLineComment)

type Parser = Parsec Void T.Text

mkDiagnostic :: UInt -> UInt -> T.Text -> Diagnostic
mkDiagnostic row column msg = Diagnostic
  { _range = Range (Position row column) (Position row column)
  , _severity = Just DiagnosticSeverity_Error
  , _code = Nothing
  , _codeDescription = Nothing
  , _source = Just "rsl-language-server"
  , _message = msg
  , _tags = Nothing
  , _relatedInformation = Just []
  , _data_ = Nothing
  }

-- Discarded output

parseHeader :: Parser ()
parseHeader = skipLineComment "rsltc version" >> void newline

parseCheckStart :: Parser ()
parseCheckStart = skipLineComment "Checking" >> void newline

parseCheckEnd :: Parser ()
parseCheckEnd = skipLineComment "Finished" >> void newline

parseSMLpath :: Parser ()
parseSMLpath = skipLineComment "SML output is in" >> void newline

parseSummary :: Parser (Int, Int)
parseSummary = do
  string "rsltc completed: "
  errs <- read <$> some numberChar
  string " error(s) "
  warns <- read <$> some numberChar
  string " warning(s)"
  optional newline
  pure (errs, warns)

parseComments :: Parser ()
parseComments = asum [parseHeader, parseCheckStart, parseCheckEnd, parseSMLpath, void parseSummary]

parseDiagnosticLine :: Parser T.Text
parseDiagnosticLine = T.pack <$> someTill asciiChar newline

parseDiagnostic :: Parser Diagnostic
parseDiagnostic = do
  lookAhead $ (oneOf ("./" :: String)) -- filepaths start with . (relative) or / (absolute)
  someTill asciiChar (string ".rsl:")
  row <- max 0 . subtract 1 . read <$> someTill digitChar (char ':')
  column <- read <$> someTill digitChar (char ':')
  hspace
  message <- parseDiagnosticLine
  extraLines <- many extraDignosticLine
  let fullMessage = T.unlines $ message : extraLines
      fullMessage' = fromMaybe fullMessage $ T.stripSuffix "\n" fullMessage
  pure $ mkDiagnostic row column fullMessage'

extraDignosticLine :: Parser T.Text
extraDignosticLine = do
  notFollowedBy (parseComments <|> void parseDiagnostic)
  parseDiagnosticLine

parseLine :: Parser [Diagnostic]
parseLine = ((: []) <$> try parseDiagnostic) <|> ((parseCheckStart <|> parseCheckEnd <|> parseSMLpath) >> pure [])

parseCorrectSyntax :: Parser [Diagnostic]
parseCorrectSyntax = between parseHeader parseSummary $ concat <$> many parseLine

parseSyntaxError :: Parser [Diagnostic]
parseSyntaxError = (: []) <$> parseDiagnostic

parseRSLTC :: T.Text -> Either (ParseErrorBundle T.Text Void) [Diagnostic]
parseRSLTC = runParser (parseCorrectSyntax <|> parseSyntaxError) ""
