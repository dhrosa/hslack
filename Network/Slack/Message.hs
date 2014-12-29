module Network.Slack.Message
       (
         Message(..),
         MessageRaw(..),
         convertRawMessage,
         TimeStamp(..),
         timeStampToString,
         channelHistory,
         channelHistoryBefore,
         channelHistoryAll,
         channelHistoryRecent,
         messagesByUser,
         postMessage
       )
       where

import Network.Slack.Prelude

import Network.Slack.Types (SlackResponseName(..), parseStrippedPrefix, Slack(..), request)

import Network.Slack.User (User(..), userFromId)
import Network.Slack.Channel (Channel(..))

import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)

import qualified Data.Traversable as T
import qualified Data.Map as M

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

instance SlackResponseName TimeStamp where
  slackResponseName _ = "ts"

-- | A message sent on a channel. Message can also mean things like user joined or a message was edited
-- TODO: Make this into a sum type of different message types, instead of using Maybe
data MessageRaw = MessageRaw {
  _messageType :: String,
  _messageUser :: Maybe String, -- user ID
  _messageText :: String,
  _messageTs :: TimeStamp
} deriving (Show, Generic)

instance FromJSON MessageRaw where
  parseJSON = parseStrippedPrefix "_message"

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
  -- This converts a Maybe (Slack User) to a Slack (Maybe User)
  user <- T.sequence (userFromId <$> muid)
  return (Message mtype user mtext mts)
   
-- | List of the past n messages in the given channel
-- n must be no greater than 1000
channelHistory :: Int -> Channel -> Slack [Message]
channelHistory n chan = mapM convertRawMessage =<< request "channels.history" args
  where
    args = M.fromList [
      ("channel", channelId chan),
      ("count", show n)
      ]

-- | Gets the n messages occuring before the given time
channelHistoryBefore :: Int -> TimeStamp -> Channel -> Slack [Message]
channelHistoryBefore n ts chan = mapM convertRawMessage =<< request "channels.history" args
  where
    args = M.fromList [
      ("channel", channelId chan),
      ("count", show n),
      ("latest", timeStampToString ts)
      ]
-- | Retrieves the entire channel history
channelHistoryAll :: Channel -> Slack [Message]
channelHistoryAll chan = do
  latest <- channelHistory 1000 chan
  let
    older = go . messageTimeStamp . last $ latest
    -- Recursively fetch older and older messages, until Slack returns an empty list
    go :: TimeStamp -> Slack [Message]
    go ts = do
      messages <- channelHistoryBefore 1000 ts chan
      case messages of
       -- No more messages!
       [] -> return []
       -- Return the retreived messages ++ the messages older than the oldest retrieved message
       _  -> (messages ++) <$> (go . messageTimeStamp . last $ messages)
  (latest ++) <$> older

-- | Retrieves a list of the most recent messages within the last n seconds
channelHistoryRecent :: Int -> Channel -> Slack [Message]
channelHistoryRecent n chan = do
  now <- liftIO getCurrentTime
  let
    args = M.fromList [
      ("channel", channelId chan),
      ("count", "1000"),
      ("oldest", timeStampToString . TimeStamp $ addUTCTime nSecsAgo now)
      ]
    -- Convert to NominalDiffTime
    nSecsAgo = fromInteger (- (toInteger n))
  mapM convertRawMessage =<< request "channels.history" args
    
                              
-- | Retrieves the messages by the given user
messagesByUser :: User -> [Message] -> [Message]
messagesByUser user = filter (byUser . messageUser)
  where
    -- Messages with no users are excluded
    byUser Nothing = False
    byUser (Just u) = u == user

-- | Posts a message as the given user to the given channel.
-- Returns the timestamp of the message, if successful
postMessage :: String -> String -> Channel -> Slack TimeStamp
postMessage uname text chan = request "chat.postMessage" args
  where
    args = M.fromList [
      ("channel", channelId chan),
      ("username", uname),
      ("text", text)
      ]
