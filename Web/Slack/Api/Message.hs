module Web.Slack.Api.Message
       (
         Message(..),
         MessageRaw(..),
         convertRawMessage,
         TimeStamp(..),
         channelHistory,
         channelHistoryBefore,
         channelHistoryAll,
         channelHistoryRecent,
         messagesByUser,
         postMessage,
         postMessage_,
         timeStampFromUtcTime,
       )
       where

import           Data.Aeson                 ( ToJSON( toJSON ) )
import qualified Data.Map              as M
import           Data.Time.Clock            ( UTCTime
                                            , addUTCTime
                                            , getCurrentTime
                                            )
import           Data.Time.Clock.POSIX      ( utcTimeToPOSIXSeconds )
import           Data.Time.Format           ( defaultTimeLocale
                                            , formatTime
                                            , parseTime
                                            )
import Data.Maybe (fromJust)
import qualified Data.Traversable      as T
import           Web.Slack.Api.Channel      ( Channel(..) )
import           Web.Slack.Api.Prelude
import           Web.Slack.Api.Types        ( Slack(..)
                                            , SlackResponseName(..)
                                            , parseStrippedPrefix
                                            , request
                                            )
import           Web.Slack.Api.User         ( User(..)
                                            , userFromId
                                            )
import qualified Data.Text as Text

-- | Fixed point number with 12 decimal places of precision
newtype TimeStamp = TimeStamp {
  slackTs :: Text
  } deriving (Show, Eq, Ord)

-- | Converts to utc time. we pretty much know it will work b/c fromJSON tries it first
utcTime :: TimeStamp -> UTCTime
utcTime = fromJust . parseTime defaultTimeLocale "%s%Q" . unpack . slackTs

timeStampFromUtcTime :: UTCTime -> TimeStamp
timeStampFromUtcTime = TimeStamp . Text.pack . formatTime defaultTimeLocale "%s%Q"

-- | Converts a TimeStamp to the timestamp format the Slack API expects
timeStampToString :: TimeStamp -> String
timeStampToString = Text.unpack . slackTs

instance FromJSON TimeStamp where
  parseJSON (String s) = do
    let maybeTime = parseTime defaultTimeLocale "%s%Q" (unpack s):: Maybe UTCTime
    case maybeTime of
     Nothing     -> fail "Incorrect timestamp format."
     Just _ -> return (TimeStamp s)

  parseJSON _ = fail "Expected a timestamp string"

instance ToJSON TimeStamp where
  toJSON (TimeStamp s) = toJSON s

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
      ("latest", Text.unpack $ slackTs ts)
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
      ("oldest", timeStampToString . timeStampFromUtcTime $ addUTCTime nSecsAgo now)
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

-- | Posts a message as the default token user to the given channel.
-- Returns the timestamp of the message, if successful
postMessage_ :: String -> Channel -> Slack TimeStamp
postMessage_ text chan = request "chat.postMessage" args
  where
    args = M.fromList [
      ("channel", channelId chan),
      ("text", text)
      ]
