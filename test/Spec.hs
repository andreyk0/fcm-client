{-# LANGUAGE OverloadedStrings          #-}

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
  encode (def :: FCMNotification) @?= "{}"

  encode (def & fcmBody ?~ "fcm body") @?=
    "{\"body\":\"fcm body\"}"

  encode (def & fcmColor ?~ "#001122") @?=
    "{\"color\":\"#001122\"}"

  encode (def & fcmTitleLocKey ?~ "loc title"
              & fcmTitleLocArgs .~ [] ) @?=
     "{\"title_loc_key\":\"loc title\"}"

  encode ( def & fcmTitleLocKey ?~ "loc title"
               & fcmTitleLocArgs .~ [FCMLocString "locStr", FCMLocNumber 1.0, FCMLocBool True]
         ) @?= "{\"title_loc_key\":\"loc title\",\"title_loc_args\":\"[\\\"locStr\\\",1,true]\"}"


  encode ( def & fcmBodyLocKey ?~ "loc body"
               & fcmBodyLocArgs .~ ["locStr", FCMLocNumber 1.0, FCMLocBool True]
         ) @?= "{\"body_loc_key\":\"loc body\",\"body_loc_args\":\"[\\\"locStr\\\",1,true]\"}"


test_messageJSON = do
  encode (def :: FCMMessage) @?= "{}"

  encode (def & fcmCollapseKey ?~ "ckey"
              & fcmTimeToLive ?~ 3
              & fcmData ?~ Map.fromList [("foo","bar")]
         ) @?= "{\"collapse_key\":\"ckey\",\"time_to_live\":3,\"data\":{\"foo\":\"bar\"}}"

  encode (def & fcmPriority .~ FCMPriorityHigh) @?=
    "{\"priority\":\"high\"}"

  encode (def & fcmPriority .~ FCMPriorityNormal) @?= "{}"

  encode (def & fcmContentAvailable .~ False) @?= "{}"

  encode (def & fcmContentAvailable .~ True) @?=
    "{\"content_available\":true}"

  decode "{\"content_available\":true}" ^.. (_Just . fcmContentAvailable) @?= [True]

  encode ( def & fcmContentAvailable .~ True
               & fcmWithNotification %~ ( (fcmBody ?~ "n body")
                                        . (fcmTitle ?~ "n title")
                                        )
         ) @?= "{\"content_available\":true,\"notification\":{\"title\":\"n title\",\"body\":\"n body\"}}"


test_responseJSON = do
  encode (FCMMessageResponse def) @?= "{\"multicast_id\":0,\"success\":0,\"failure\":0,\"canonical_ids\":0}"

  encode (FCMTopicResponseOk def) @?= "{\"message_id\":0}"

  decode "{\"message_id\":6538060933731373360}" ^..
    (_Just . _FCMTopicResponse . _FCMTopicResponseOk . fcmTopicMessageId ) @?= [6538060933731373360]

  let jRes = "{\"multicast_id\":4984205480219716339,\"success\":0,\"failure\":1,\"canonical_ids\":0,\"results\":[{\"error\":\"NotRegistered\"}]}"

  decode jRes ^..
    (_Just . _FCMMessageResponse . fcmResults . _Just . traverse .  _FCMMessageResponseResultError ) @?=
      [FCMErrorNotRegistered]

  decode jRes ^.. (_Just . _FCMMessageResponse . fcmMulticastId ) @?= [4984205480219716339]
