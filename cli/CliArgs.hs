{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}


module CliArgs (
  CliArgs(..)
, CliCmd(..)
, runWithArgs
) where


import           Control.Lens
import           Data.Monoid
import qualified Data.Text as T
import           FCMClient.Types
import           Options.Applicative
import           System.Environment


data CliArgs = CliArgs { cliAuthKey:: String
                       , cliCmd:: CliCmd
                       }


data CliCmd = CliCmdSendJsonBatch (Maybe FilePath)
            | CliCmdSendMessage (FCMMessage -> FCMMessage)


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
  <*> subparser ( command "message" (info (helper <*> parseCliCmdSendMessage)
                                          (progDesc "Send test message."))

               <> command "batch" (info  (helper <*> (pure (CliCmdSendJsonBatch Nothing)))
                                         (progDesc "Send message batch."))
                )

parseCliCmdSendMessage :: Parser CliCmd
parseCliCmdSendMessage = CliCmdSendMessage
  <$> (    fmap ((set fcmTo) . Just . T.pack)
                (strOption ( long "to"
                          <> short 't'
                          <> help "message to (reg token, notification key or topic)"))

      <..> fmap ((set (fcmWithNotification . fcmTitle)) . Just . T.pack)
                (strOption ( long "title"
                          <> short 'T'
                          <> help "notification title text"))

      <..> fmap ((set (fcmWithNotification . fcmBody)) . Just . T.pack)
                (strOption ( long "body"
                          <> short 'b'
                          <> help "notification body text"))
      )


-- | Chains lenses through Applicative
(<..>) :: (Applicative f)
       => f (b -> c)
       -> f (a -> b)
       -> f (a -> c)
(<..>) bc ab = pure (.) <*> bc <*> ab


runWithArgs:: (CliArgs -> IO ())
           -> IO ()
runWithArgs rwa = do
  maybeAuthKey <- lookupEnv "FCM_AUTH_KEY"

  let opts = info (helper <*> parseArgs maybeAuthKey) ( fullDesc
               <> progDesc "Simple FCM CLI client, send a test message or a JSON batch from a file."
               <> header "Simple FCM CLI client, send a test message or a JSON batch from a file."
               )

  execParser opts >>= rwa
