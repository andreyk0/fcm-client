{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module Main where


import           CliArgs
import           Data.Aeson(Object, encode)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import           FCMClient
import           FCMClient.Types
import           Network.HTTP.Client


main :: IO ()
main = runWithArgs $ \CliArgs{..} -> do

  let sendMessage msgMod = do
        let msg = msgMod newFCMMessage
        putStrLn $ (LUTF8.toString . encode) msg
        res <- fcmCallJSON (UTF8.fromString cliAuthKey) msg :: IO (Response Object)
        putStrLn $ show res
        putStrLn $ (LUTF8.toString . encode) (responseBody res)

  case cliCmd
    of CliCmdSendMessage msgMod -> sendMessage msgMod
       CliCmdSendJsonBatch _ -> undefined

