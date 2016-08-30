{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}


module CliArgs (
  CliArgs(..)
, runWithArgs
) where


import           Data.Monoid
import           Options.Applicative
import           System.Environment


data CliArgs = CliArgs { cliAuthKey:: String
                       , cliCmd:: CliCmd
                       } deriving (Eq, Show)


data CliCmd = CliCmdSendJsonBatch (Maybe FilePath)
            | CliCmdSendMessage
            deriving (Eq, Show)


parseArgs :: Maybe String -- ^ default auth key from shell env
          -> Parser CliArgs
parseArgs maybeAuthKey = CliArgs
  <$> strOption
      ( long "auth-key"
     <> short 'k'
     <> ( case maybeAuthKey
            of Nothing -> mempty
               Just ak -> value ak
        )
     <> help "Auth key, defaults to FCM_AUTH_KEY environmental variable." )
  <*> subparser ( command "message"
                    (info (pure CliCmdSendMessage)
                      (progDesc "Send test message."))
               <> command "batch"
                    (info (pure (CliCmdSendJsonBatch Nothing))
                      (progDesc "Send message batch."))
                )


runWithArgs:: (CliArgs -> IO ())
           -> IO ()
runWithArgs rwa = do
  maybeAuthKey <- lookupEnv "FCM_AUTH_KEY"

  let opts = info (helper <*> parseArgs maybeAuthKey) ( fullDesc
               <> progDesc "Simple FCM CLI client, send a test message or a JSON batch from a file."
               <> header "Simple FCM CLI client, send a test message or a JSON batch from a file."
               )

  execParser opts >>= rwa
