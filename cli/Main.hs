{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}


module Main where


import           CliArgs
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Monoid
import           FCMClient
import           FCMClient.Types
import           Network.HTTP.Client
import           Network.HTTP.Types
import           System.IO


main :: IO ()
main = runWithArgs $ \CliArgs{..} -> do

  let sendMessage msgMod = do
        let msg = msgMod newFCMMessage
        putStrLn $ (LUTF8.toString . encode) msg
        res <- fcmCallJSON (UTF8.fromString cliAuthKey) msg :: IO (Response Object)
        putStrLn $ show res
        putStrLn $ (LUTF8.toString . encode) (responseBody res)

      sendMessageBatch CliJsonBatchArgs{..} = do
        batchInputConduit cliBatchInput
          =$= parseInputConduit =$= callFCMConduit (UTF8.fromString cliAuthKey)
          =$= encodeOutputConduit
          $$ batchOutputConduit cliBatchOutput


  case cliCmd
    of CliCmdSendMessage msgMod -> sendMessage msgMod
       CliCmdSendJsonBatch bargs -> runResourceT $ sendMessageBatch bargs



-- | Attempts to parse input, one JSON object per line,
-- either succeeds and gives result or fails and gives json-serializable error.
--
-- Input can contain JSON fields that are not FCM-related, they'll be stripped out when we make
-- an FCM request but original input will be propagated to the output, this allows for addition
-- of request tracking/debugging fields that makes it easier to interpret results.
parseInputConduit :: (MonadIO m, MonadResource m)
                  => Conduit BS.ByteString m (Either String (Value, FCMMessage))
parseInputConduit =
  CB.lines =$= CL.map (eitherDecode' . LBS.fromStrict) =$=
    ( CL.map $ \ejObj -> do jObj <- ejObj
                            case fromJSON jObj
                              of Success m -> Right (jObj, m)
                                 Error e   -> Left e )

encodeOutputConduit :: (MonadIO m, MonadResource m)
                    => Conduit Value m BS.ByteString
encodeOutputConduit =
  CL.map (LBS.toStrict . encode)
    =$= ( awaitForever $ \l -> do yield l
                                  yield "\n" )


-- | Convert each input line into a JSON object containing original input and results of the call.
callFCMConduit :: (MonadIO m, MonadResource m)
               => BS.ByteString -- ^ authorization key
               -> Conduit (Either String (Value, FCMMessage)) m Value
callFCMConduit authKey = CL.mapM $ \input ->
  case input
    of Left e  -> return $ object [ ("type", "ParserError")
                                  , ("error", toJSON e)
                                  ]
       Right m -> liftIO $ sendMessage m

  where sendMessage :: (Value, FCMMessage) -> IO Value
        sendMessage (jm,m) = do
          res <- fcmCallJSON authKey m

          let mkRes t = object [ ("type", t)
                               , ("message", jm)
                               , ("response", responseBody res)
                               ]

          return $ if ( responseStatus res == status200)
                   then mkRes "Success"
                   else mkRes "ServerError"




batchInputConduit :: (MonadIO m, MonadResource m)
                  => Maybe FilePath
                  -> Producer m BS.ByteString
batchInputConduit (Just fp) = CB.sourceFile fp
batchInputConduit Nothing = do
  liftIO $ do hSetBinaryMode stdin True
              hSetBuffering stdin (BlockBuffering Nothing)
  CB.sourceHandle stdin


batchOutputConduit :: (MonadIO m, MonadResource m)
                  => Maybe FilePath
                  -> Consumer BS.ByteString m ()
batchOutputConduit (Just fp) = CB.sinkFile fp
batchOutputConduit Nothing = do
  liftIO $ do hSetBinaryMode stdout True
              hSetBuffering stdout (BlockBuffering Nothing)
  CB.sinkHandle stdout
