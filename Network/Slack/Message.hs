module Network.Slack.Message
       (
         Message(..),
         TimeStamp(..),
         timeStampToString
       )
       where

import Network.Slack.Types (Generic, FromJSON(..), SlackResponseName(..), parseResponse, Slack(..), request)

import Network.Slack.User (User(..), userFromId, userFromName)
import Network.Slack.Channel (Channel(..), channelFromName)

import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)

import Data.Text (unpack)

import Data.Aeson(Value(..))

import qualified Data.Traversable as T
import qualified Data.Map as M

import Control.Applicative ((<$>))

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

-- | Converts a MessageRaw into a Message
convertRawMessage :: MessageRaw -> Slack Message
convertRawMessage (MessageRaw mtype muid mtext mts) = do
  user <- T.sequence (userFromId <$> muid)
  return (Message mtype user mtext mts)
   
-- | List of the past 1000 messages in the given channel
channelHistory :: Channel -> Slack [Message]
channelHistory chan = mapM convertRawMessage =<< request "channels.history" args
  where
    args = M.fromList [
      ("channel", channelId chan),
      ("count", "1000")
      ]

-- | Gets the 1000 messages occuring before the given time
channelHistoryBefore :: TimeStamp -> Channel -> Slack [Message]
channelHistoryBefore ts chan = mapM convertRawMessage =<< request "channels.history" args
  where
    args = M.fromList [
      ("channel", channelId chan),
      ("count", "1000"),
      ("latest", timeStampToString ts)
      ]
-- | Retrieves the entire channel history
channelHistoryAll :: Channel -> Slack [Message]
channelHistoryAll chan = do
  latest <- channelHistory chan
  let
    older = go . messageTimeStamp . last $ latest
    -- Recursively fetch older and older messages, until Slack returns an empty list
    go :: TimeStamp -> Slack [Message]
    go ts = do
      messages <- channelHistoryBefore ts chan
      case messages of
       -- No more messages!
       [] -> return []
       -- Return the retreived messages ++ the messages older than the oldest retrieved message
       _  -> (messages ++) <$> (go . messageTimeStamp . last $ messages)
  (latest ++) <$> older
           
-- | Retrieves the messages by the given user
messagesByUser :: User -> [Message] -> [Message]
messagesByUser user = filter (byUser . messageUser)
  where
    -- Messages with no users are excluded
    byUser Nothing = False
    byUser (Just u) = u == user

-- | Retrieves all of brandon's posts in #general
brandonHistory :: Slack String
brandonHistory = do
  brandon <- userFromName "brandon"
  general <- channelFromName "general"
  messages <- messagesByUser brandon <$> channelHistoryAll general
  return . unlines . map messageText $ messages
