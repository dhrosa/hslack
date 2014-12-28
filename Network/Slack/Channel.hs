module Network.Slack.Channel
       (ChannelRaw(..),
        Channel(..)
       )
       where

import Network.Slack.Monad (User(..))
import Network.Slack.Types (FromJSON(..), Generic, SlackResponseName(..), parseResponse)

-- | Represents a response from the "channels.list" command. This
-- contains a list of user IDs instead of user objects. This should be
-- converted to a Channel object
data ChannelRaw = ChannelRaw {
  channelRawId :: String,
  channelRawName :: String,
  channelRawMembers :: [String] -- This is a list of user IDs
  } deriving (Show, Generic)

instance FromJSON ChannelRaw where
  parseJSON = parseResponse "channelRaw"

instance SlackResponseName [ChannelRaw] where
  slackResponseName _ = "channels"

-- | A more useable version of ChannelRaw
data Channel = Channel {
  channelId :: String,
  channelName :: String,
  channelMembers :: [User]
} deriving (Show)
