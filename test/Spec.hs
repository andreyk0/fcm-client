{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
where


import           Control.Lens
import           Data.Aeson
import           Data.Default.Class
import qualified Data.Map as Map
import           FCMClient.Types
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit


main = defaultMain tests

tests = [ testGroup "JSON" [
            testCase "notification"   test_notificationJSON
          , testCase "message"        test_messageJSON
          , testCase "response"       test_responseJSON
          ]
        ]

test_notificationJSON = do
  (encode (def :: FCMNotification)) @?= "{}"

  (encode $ fcmBody .~ (Just "fcm body") $ def) @?=
    "{\"body\":\"fcm body\"}"

  (encode $ fcmColor .~ (Just "#001122") $ def) @?=
    "{\"color\":\"#001122\"}"

  (encode $ fcmTitleLocKey .~ (Just $ "loc title") $
              fcmTitleLocArgs .~ ([]) $
              def) @?=
     "{\"title_loc_key\":\"loc title\"}"

  (encode $ fcmTitleLocKey .~ (Just $ "loc title") $
              fcmTitleLocArgs .~ ([FCMLocString "locStr", FCMLocNumber 1.0, FCMLocBool True]) $
              def) @?=
     "{\"title_loc_key\":\"loc title\",\"title_loc_args\":\"[\\\"locStr\\\",1,true]\"}"


  (encode $ fcmBodyLocKey .~ (Just $ "loc body") $
              fcmBodyLocArgs .~ (["locStr", FCMLocNumber 1.0, FCMLocBool True]) $
              def) @?=
     "{\"body_loc_key\":\"loc body\",\"body_loc_args\":\"[\\\"locStr\\\",1,true]\"}"


test_messageJSON = do
  (encode (def :: FCMMessage)) @?= "{}"

  (encode $ fcmCollapseKey .~ (Just "ckey") $
            fcmTimeToLive .~ (Just 3) $
            fcmData .~ (Just (Map.fromList [("foo","bar")])) $
            def) @?=
    "{\"collapse_key\":\"ckey\",\"time_to_live\":3,\"data\":{\"foo\":\"bar\"}}"

  (encode $ fcmPriority .~ FCMPriorityHigh $ def) @?=
    "{\"priority\":\"high\"}"

  (encode $ fcmPriority .~ FCMPriorityNormal $ def) @?= "{}"

  (encode $ fcmContentAvailable .~ False $ def) @?= "{}"

  (encode $ fcmContentAvailable .~ True $ def) @?= "{\"content_available\":true}"

  (decode "{\"content_available\":true}") ^.. (_Just . fcmContentAvailable) @?= [True]

  (encode $ def &
           ( (fcmContentAvailable .~ True)
           . ( fcmWithNotification %~ ( (fcmBody .~ Just "n body")
                                      . (fcmTitle .~ Just "n title")
                                      )
             )
           )
    ) @?= "{\"content_available\":true,\"notification\":{\"title\":\"n title\",\"body\":\"n body\"}}"


test_responseJSON = do
  (encode (FCMMessageResponse def)) @?= "{\"multicast_id\":0,\"success\":0,\"failure\":0,\"canonical_ids\":0}"

  (encode (FCMTopicResponseOk def)) @?= "{\"message_id\":0}"

  (decode "{\"message_id\":6538060933731373360}") ^..
    (_Just . _FCMTopicResponse . _FCMTopicResponseOk . fcmTopicMessageId ) @?= [6538060933731373360]

  let jRes = "{\"multicast_id\":4984205480219716339,\"success\":0,\"failure\":1,\"canonical_ids\":0,\"results\":[{\"error\":\"NotRegistered\"}]}"

  (decode jRes) ^..
    (_Just . _FCMMessageResponse . fcmResults . _Just . traverse .  _FCMMessageResponseResultError ) @?=
      [FCMErrorNotRegistered]

  (decode jRes) ^.. (_Just . _FCMMessageResponse . fcmMulticastId ) @?= [4984205480219716339]
