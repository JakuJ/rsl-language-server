{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main where

import           Control.Monad.IO.Class
import           Language.LSP.Server
import           Raise.Handlers

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }
