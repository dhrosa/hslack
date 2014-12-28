module Network.Slack.Message
       (
         MessageRaw(..),
         Message(..),
         TimeStamp(..),
         timeStampToString
       )
       where

import Network.Slack.Monad (User(..))
import Network.Slack.Types (Generic, FromJSON(..), SlackResponseName(..), parseResponse)

import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)

import Data.Text (unpack)

import Data.Aeson(Value(..))

-- | Fixed point number with 12 decimal places of precision
newtype TimeStamp = TimeStamp {
  utcTime :: UTCTime
  } deriving (Show, Eq, Ord)

-- | Converts a TimeStamp to the timestamp format the Slack API expects
timeStampToString :: TimeStamp -> String
timeStampToString = formatTime defaultTimeLocale "%s%Q" . utcTime

instance FromJSON TimeStamp where
  parseJSON (String s) = do
    let maybeTime = parseTime defaultTimeLocale "%s%Q" (unpack s):: Maybe UTCTime
    case maybeTime of
     Nothing     -> fail "Incorrect timestamp format."
     Just (time) -> return (TimeStamp time)

  parseJSON _ = fail "Expected a timestamp string"

-- | A message sent on a channel. Message can also mean things like user joined or a message was edited
-- TODO: Make this into a sum type of different message types, instead of using Maybe
data MessageRaw = MessageRaw {
  messageRawType :: String,
  messageRawUser :: Maybe String, -- user ID
  messageRawText :: String,
  messageRawTs :: TimeStamp
} deriving (Show, Generic)

instance FromJSON MessageRaw where
  parseJSON = parseResponse "messageRaw"

instance SlackResponseName [MessageRaw] where 
  slackResponseName _ = "messages"

-- | A nicer version of MessageRaw, with the user id converted to a User
data Message = Message {
  messageType :: String,
  messageUser :: Maybe User,
  messageText :: String,
  messageTimeStamp :: TimeStamp
  } deriving (Show, Eq) 
