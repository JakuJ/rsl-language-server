module ArgParse (
  parseArgs,
  Args(..),
) where

import           Options.Applicative

data Args = Args
  { compile :: Bool
  , version :: Bool
  }

parseArgs :: IO Args
parseArgs = execParser opts

opts :: ParserInfo Args
opts = info (parser <**> helper) desc
  where
    desc = fullDesc <> progDesc "Language server implementation for the RAISE Specification Language."

parser :: Parser Args
parser = Args <$> parseCompile <*> parseVersion

parseCompile :: Parser Bool
parseCompile = switch $ long "compile"
                <> short 'c'
                <> help "Whether to run the compiler (rsltc -m) in addition to the type checker"

parseVersion :: Parser Bool
parseVersion = switch $ long "version"
                <> short 'v'
                <> help "Show version"
