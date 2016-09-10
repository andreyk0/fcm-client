{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


-- | Google Firebase Cloud Messaging model / JSON conversions.
--   https://firebase.google.com/docs/cloud-messaging/http-server-ref
--
--   Date types in this module map field for field in the google docs.
--   See FCMClient.Types module for some wrapper functions on top of raw JSON types.
--
module FCMClient.JSON.Types (
  FCMData
, FCMNotification
, newFCMNotification
, fcmTitle
, fcmBody
, fcmIcon
, fcmSound
, fcmTag
, fcmColor
, fcmBadge
, fcmClickAction
, fcmBodyLocKey
, fcmBodyLocArgs
, fcmTitleLocKey
, fcmTitleLocArgs
, FCMMessage
, newFCMMessage
, fcmTo
, fcmRegistrationIDs
, fcmCondition
, fcmCollapseKey
, fcmPriority
, fcmContentAvailable
, fcmDelayWhileIdle
, fcmTimeToLive
, fcmRestrictedPackageName
, fcmDryRun
, fcmData
, fcmNotification
) where


import           Control.Lens.TH
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import           Data.Text (Text)


type FCMData = Map Text Text


-- | FCM Notification as defined in https://firebase.google.com/docs/cloud-messaging/http-server-ref#notification-payload-support
-- Abstract type, use lens API to access fields. Record fields are kept private and used for JSON conversion.
data FCMNotification =
  FCMNotification {

    -- | title  Optional, string
    -- Indicates notification title. This field is not visible on iOS phones and tablets.
    _fcmTitle :: !(Maybe Text)

    -- | body  Optional, string
    -- Indicates notification body text.
  , _fcmBody :: !(Maybe Text)

    -- | icon   Optional, string
    -- Android: Indicates notification icon. Sets value to myicon for drawable resource myicon.
  , _fcmIcon :: !(Maybe Text)

    -- | sound  Optional, string
    -- IOS: Indicates a sound to play when the device receives a notification. Sound
    -- files can be in the main bundle of the client app or in the
    -- Library/Sounds folder of the app's data container. See the iOS Developer
    -- Library for more information.
    -- Android: Indicates a sound to play when the device receives a
    -- notification. Supports default or the filename of a sound resource
    -- bundled in the app. Sound files must reside in /res/raw/.
  , _fcmSound :: !(Maybe Text)

    -- | tag   Optional, string
    -- Android: Indicates whether each notification results in a new entry in the
    -- notification drawer on Android.  If not set, each request creates a new
    -- notification. If set, and a notification with the same tag is already being
    -- shown, the new notification replaces the existing one in the notification
    -- drawer.
  , _fcmTag :: !(Maybe Text)

    -- | color   Optional, string
    -- Android: Indicates color of the icon, expressed in #rrggbb format
  , _fcmColor:: !(Maybe Text)

    -- | badge  Optional, string
    -- Indicates the badge on the client app home icon.
  , _fcmBadge :: !(Maybe Text)

    -- | click_action  Optional, string
    -- IOS: Indicates the action associated with a user click on the notification.
    -- Corresponds to category in the APNs payload.
    -- Android: Indicates the action associated with a user click on the
    -- notification. When this is set, an activity with a matching intent
    -- filter is launched when user clicks the notification.
  , _fcmClickAction :: !(Maybe Text)

    -- | body_loc_key  Optional, string
    -- IOS: Indicates the key to the body string for localization. Corresponds to
    -- "loc-key" in the APNs payload.
    -- Android: Indicates the key to the body string for localization. Use the
    -- key in the app's string resources when populating this value.
  , _fcmBodyLocKey :: !(Maybe Text)

    -- | body_loc_args  Optional, JSON array as string
    -- IOS: Indicates the string value to replace format specifiers in the body string
    -- for localization. Corresponds to "loc-args" in the APNs payload.
    -- Android: Indicates the string value to replace format specifiers in the
    -- body string for localization. For more information, see Formatting and
    -- Styling.
  , _fcmBodyLocArgs :: !(Maybe Text)

    -- | title_loc_key  Optional, string
    -- IOS: Indicates the key to the title string for localization. Corresponds to
    -- "title-loc-key" in the APNs payload.
    -- Android: Indicates the key to the title string for localization. Use the
    -- key in the app's string resources when populating this value.
  , _fcmTitleLocKey :: !(Maybe Text)


    -- | title_loc_args  Optional, JSON array as string
    -- IOS: Indicates the string value to replace format specifiers in the title
    -- string for localization.Corresponds to "title-loc-args" in the APNs
    -- payload.
    -- Android: Indicates the string value to replace format specifiers in the
    -- title string for localization. For more information, see Formatting
    -- strings.
  , _fcmTitleLocArgs:: !(Maybe Text)
  } deriving (Eq, Show)

$(makeLenses ''FCMNotification)
$(deriveJSON (aesonPrefix snakeCase) { omitNothingFields = True } ''FCMNotification)



newFCMNotification :: FCMNotification
newFCMNotification = FCMNotification Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-- | FCM Message as defined in https://firebase.google.com/docs/cloud-messaging/http-server-ref#send-downstream
-- Abstract type, use lens API to access fields. Record fields are kept private and used for JSON conversion.
data FCMMessage =
  FCMMessage {
    -- | to  Optional, string
    -- This parameter specifies the recipient of a message.
    -- The value must be a registration token, notification key, or topic.
    -- Do not set this field when sending to multiple topics. See condition.
    _fcmTo :: !(Maybe Text)

    -- | registration_ids String array
    -- This parameter specifies a list of devices (registration tokens, or IDs)
    -- receiving a multicast message. It must contain at least 1 and at most 1000
    -- registration tokens.
    -- Use this parameter only for multicast messaging, not for single
    -- recipients. Multicast messages (sending to more than 1 registration tokens) are
    -- allowed using HTTP JSON format only.
  , _fcmRegistrationIDs :: !(Maybe (NonEmpty Text))

    -- | condition  Optional, string
    -- This parameter specifies a logical expression of conditions that determine
    -- the message target.  Supported condition: Topic, formatted as "'yourTopic' in
    -- topics". This value is case-insensitive.  Supported operators: &&, ||. Maximum
    -- two operators per topic message supported.
  , _fcmCondition :: !(Maybe Text)

    -- | collapse_key  Optional, string
    -- This parameter identifies a group of messages (e.g., with collapse_key:
    -- "Updates Available") that can be collapsed, so that only the last message gets
    -- sent when delivery can be resumed. This is intended to avoid sending too many
    -- of the same messages when the device comes back online or becomes active (see
    -- delay_while_idle).
    -- Note that there is no guarantee of the order in which messages get sent.
    -- Note: A maximum of 4 different collapse keys is allowed at any given time.
    -- This means a FCM connection server can simultaneously store 4 different
    -- send-to-sync messages per client app. If you exceed this number, there is no
    -- guarantee which 4 collapse keys the FCM connection server will keep.
  , _fcmCollapseKey :: !(Maybe Text)

    -- | priority  Optional, string
    -- Sets the priority of the message. Valid values are "normal" and "high." On
    -- iOS, these correspond to APNs priorities 5 and 10.  By default, messages are
    -- sent with normal priority. Normal priority optimizes the client app's
    -- battery consumption and should be used unless immediate delivery is
    -- required. For messages with normal priority, the app may receive the message
    -- with unspecified delay.  When a message is sent with high priority, it is
    -- sent immediately, and the app can wake a sleeping device and open a network
    -- connection to your server.  For more information, see Setting the priority
    -- of a message.
  , _fcmPriority :: !(Maybe Text)

    -- | content_available  Optional, JSON boolean
    -- On iOS, use this field to represent content-available in the APNS
    -- payload. When a notification or message is sent and this is set to true,
    -- an inactive client app is awoken. On Android, data messages wake the app
    -- by default. On Chrome, currently not supported.
  , _fcmContentAvailable :: !(Maybe Bool)

    -- | delay_while_idle  Optional, JSON boolean
    -- When this parameter is set to true, it indicates that the message should
    -- not be sent until the device becomes active. The default value is false.
  , _fcmDelayWhileIdle :: !(Maybe Bool)

    -- | time_to_live  Optional, JSON number
    -- This parameter specifies how long (in seconds) the message should be kept in
    -- FCM storage if the device is offline. The maximum time to live supported is
    -- 4 weeks, and the default value is 4 weeks. For more information, see Setting
    -- the lifespan of a message.
  , _fcmTimeToLive :: !(Maybe Word)

    -- | restricted_package_name  Optional, string
    -- This parameter specifies the package name of the application where the
    -- registration tokens must match in order to receive the message.
  , _fcmRestrictedPackageName :: !(Maybe Text)

    -- | dry_run  Optional, JSON boolean
    -- This parameter, when set to true, allows developers to test a request
    -- without actually sending a message. The default value is false.
  , _fcmDryRun :: !(Maybe Bool)

  -- Payload

    -- | data  Optional, JSON object
    --  This parameter specifies the custom key-value pairs of the message's
    --  payload.  For example, with data:{"score":"3x1"}: On iOS, if the message is
    --  sent via APNS, it represents the custom data fields. If it is sent via FCM
    --  connection server, it would be represented as key value dictionary in
    --  AppDelegate application:didReceiveRemoteNotification:.  On Android, this
    --  would result in an intent extra named score with the string value 3x1.  The
    --  key should not be a reserved word ("from" or any word starting with
    --  "google" or "gcm"). Do not use any of the words defined in this table (such
    --  as collapse_key).  Values in string types are recommended. You have to
    --  convert values in objects or other non-string data types (e.g., integers or
    --  booleans) to string.
  , _fcmData :: !(Maybe FCMData)

    -- | notification  Optional, JSON object
    -- This parameter specifies the predefined, user-visible key-value pairs of
    -- the notification payload. See Notification payload support for detail.
    -- For more information about notification message and data message
    -- options, see Payload.
  , _fcmNotification :: !(Maybe FCMNotification)
  } deriving (Eq, Show)


$(makeLenses ''FCMMessage)
$(deriveJSON (aesonPrefix snakeCase) { omitNothingFields = True } ''FCMMessage)


newFCMMessage :: FCMMessage
newFCMMessage = FCMMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
