module ArgParse (
  parseArgs
) where

import           Options.Applicative

parseArgs :: IO Bool
parseArgs = execParser opts

opts :: ParserInfo Bool
opts = info (compile <**> helper) desc
  where
    desc = fullDesc <> progDesc "Language server implementation for the RAISE Specification Language."

compile :: Parser Bool
compile = switch $ long "compile"
                <> short 'c'
                <> help "Whether to run the compiler (rsltc -m) in addition to the type checker"

