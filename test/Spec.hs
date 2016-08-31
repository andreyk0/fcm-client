{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
where


import           Control.Lens
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Maybe
import           FCMClient.Types
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit


main = defaultMain tests

tests = [ testGroup "JSON" [
            testCase "notification"   test_notificationJSON
          , testCase "message"        test_messageJSON
          ]
        ]

test_notificationJSON = do
  (encode newFCMNotification) @?= "{}"

  (encode $ fcmBody .~ (Just "fcm body") $ newFCMNotification) @?=
    "{\"body\":\"fcm body\"}"

  (encode $ fcmColor .~ (Just "#001122") $ newFCMNotification) @?=
    "{\"color\":\"#001122\"}"

  (encode $ fcmTitleLocKey .~ (Just $ "loc title") $
              fcmTitleLocArgs .~ ([]) $
              newFCMNotification) @?=
     "{\"title_loc_key\":\"loc title\"}"

  (encode $ fcmTitleLocKey .~ (Just $ "loc title") $
              fcmTitleLocArgs .~ ([FCMLocString "locStr", FCMLocNumber 1.0, FCMLocBool True]) $
              newFCMNotification) @?=
     "{\"title_loc_key\":\"loc title\",\"title_loc_args\":\"[\\\"locStr\\\",1,true]\"}"


  (encode $ fcmBodyLocKey .~ (Just $ "loc body") $
              fcmBodyLocArgs .~ (["locStr", FCMLocNumber 1.0, FCMLocBool True]) $
              newFCMNotification) @?=
     "{\"body_loc_key\":\"loc body\",\"body_loc_args\":\"[\\\"locStr\\\",1,true]\"}"


test_messageJSON = do
  (encode newFCMMessage) @?= "{}"

  (encode $ fcmCollapseKey .~ (Just "ckey") $
            fcmTimeToLive .~ (Just 3) $
            fcmData .~ (Just (Map.fromList [("foo","bar")])) $
            newFCMMessage) @?=
    "{\"collapse_key\":\"ckey\",\"time_to_live\":3,\"data\":{\"foo\":\"bar\"}}"

  (encode $ fcmPriority .~ FCMPriorityHigh $ newFCMMessage) @?=
    "{\"priority\":\"high\"}"

  (encode $ fcmPriority .~ FCMPriorityNormal $ newFCMMessage) @?= "{}"

  (encode $ fcmContentAvailable .~ False $ newFCMMessage) @?= "{}"

  (encode $ fcmContentAvailable .~ True $ newFCMMessage) @?= "{\"content_available\":true}"

  (toListOf fcmContentAvailable ((fromJust . decode) "{\"content_available\":true}")) @?= [True]

  (encode $ newFCMMessage &
           ( (fcmContentAvailable .~ True)
           . ( fcmWithNotification %~ ( (fcmBody .~ Just "n body")
                                      . (fcmTitle .~ Just "n title")
                                      )
             )
           )
    ) @?= "{\"content_available\":true,\"notification\":{\"title\":\"n title\",\"body\":\"n body\"}}"
