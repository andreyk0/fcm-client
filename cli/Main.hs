{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}


module Main where


import           CliArgs
import qualified Control.Concurrent.Async as A
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Conduit
import           Data.Conduit.Async
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
        let buf c = buffer' cliBatchConc c

        (batchInputConduit cliBatchInput =$= parseInputConduit)
          `buf`
          (callFCMConduit (UTF8.fromString cliAuthKey) =$= runInParallel cliBatchConc)
          `buf`
          (encodeOutputConduit =$= batchOutputConduit cliBatchOutput)


  case cliCmd
    of CliCmdSendMessage msgMod -> sendMessage msgMod
       CliCmdSendJsonBatch bargs -> runResourceT $ runCConduit $ sendMessageBatch bargs



-- | Attempts to parse input, one JSON object per line,
-- either succeeds and gives result or fails and gives json-serializable error.
--
-- Input can contain JSON fields that are not FCM-related, they'll be stripped out when we make
-- an FCM request but original input will be propagated to the output, this allows for addition
-- of request tracking/debugging fields that makes it easier to interpret results.
parseInputConduit :: (MonadIO m)
                  => Conduit BS.ByteString m (Either (BS.ByteString, String) (Value, FCMMessage))
parseInputConduit = CB.lines =$= CL.map (\line -> do
  jObj <- case eitherDecode' $ LBS.fromStrict line
            of Right v -> Right v
               Left  e -> Left (line, e)
  case fromJSON jObj
    of Success m -> Right (jObj, m)
       Error e   -> Left (line, e)
  )


encodeOutputConduit :: (MonadIO m)
                    => Conduit Value m BS.ByteString
encodeOutputConduit =
  CL.map (LBS.toStrict . encode)
    =$= ( awaitForever $ \l -> do yield l
                                  yield "\n" )


-- | Convert each input line into a JSON object containing original input and results of the call.
callFCMConduit :: (MonadIO m, MonadResource m)
               => BS.ByteString -- ^ authorization key
               -> Conduit (Either (BS.ByteString,String) (Value, FCMMessage)) m (A.Async Value)
callFCMConduit authKey = CL.mapM $ \input -> liftIO . A.async $
  case input
    of Left (i,e) -> return $ object [ ("type", "ParserError")
                                     , ("error", toJSON e)
                                     , ("input", toJSON (UTF8.toString i))
                                     ]
       Right m    -> recovering retPolicy [logRet] (const $ sendMessage m)

  where retPolicy = constantDelay 1000000 <> limitRetries 5

        logRet = logRetries (\ (_ :: HttpException) -> return True)
                            (\ _ e _ -> liftIO $ hPutStrLn stderr $ "HTTP error: " <> (show e))

        sendMessage :: (Value, FCMMessage) -> IO Value
        sendMessage (jm,m) = do
          res <- fcmCallJSON authKey m

          let mkRes t = object [ ("type", t)
                               , ("message", jm)
                               , ("response", responseBody res)
                               ]

          return $ if ( responseStatus res == status200)
                   then mkRes "Success"
                   else mkRes "ServerError"



runInParallel :: (MonadIO m)
              => Int -- ^ level
              -> Conduit (A.Async a) m a
runInParallel n = parC []
  where parC !xs = do
          let moreCnt = n - (length xs)
          moreXs <- CL.take moreCnt
          let xs' = xs ++ moreXs
          if null xs'
          then return ()
          else do (a,res) <- liftIO $ A.waitAny xs'
                  yield res
                  parC $ filter (/= a) xs'



batchInputConduit :: (MonadResource m)
                  => Maybe FilePath
                  -> Producer m BS.ByteString
batchInputConduit (Just fp) = CB.sourceFile fp
batchInputConduit Nothing = do
  liftIO $ do hSetBinaryMode stdin True
              hSetBuffering stdin (BlockBuffering Nothing)
  CB.sourceHandle stdin


batchOutputConduit :: (MonadResource m)
                  => Maybe FilePath
                  -> Consumer BS.ByteString m ()
batchOutputConduit (Just fp) = CB.sinkFile fp
batchOutputConduit Nothing = do
  liftIO $ do hSetBinaryMode stdout True
              hSetBuffering stdout (BlockBuffering Nothing)
  CB.sinkHandle stdout
