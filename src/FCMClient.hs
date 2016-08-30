{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | Firebase Cloud Messaging google client.
-- https://firebase.google.com/docs/cloud-messaging/concept-options#notifications_and_data_messages
module FCMClient (
  fcmCallJSON
, fcmJSONRequest
) where


import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types


-- | Makes an FCM JSON request, expects a JSON response.
--   https://firebase.google.com/docs/cloud-messaging/http-server-ref#send-downstream
fcmCallJSON :: (J.ToJSON req, J.FromJSON res)
            => B.ByteString -- ^ authorization key
            -> req -- ^ FCM JSON message, a typed model or a document object
            -> IO (Response res)
fcmCallJSON authKey fcmMessage =
  httpJSON (fcmJSONRequest authKey (J.encode fcmMessage))


-- | Constructs an FCM JSON request, body and additional parameters such as
--   proxy or http manager can be set for a customized HTTP call.
fcmJSONRequest :: B.ByteString -- ^ authorization key
               -> L.ByteString -- ^ JSON POST data
               -> Request
fcmJSONRequest authKey jsonBytes =
  "https://fcm.googleapis.com/fcm/send"
    { method = "POST"
    , requestHeaders = [ (hAuthorization, "key=" <> authKey)
                       , (hContentType, "application/json")
                       ]
    , requestBody = RequestBodyLBS jsonBytes
    }
