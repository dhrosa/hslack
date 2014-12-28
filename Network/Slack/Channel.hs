module Network.Slack.Channel
       (
        Channel(..),
        channels,
        channelFromName
       )
       where

import Network.Slack.Types (FromJSON(..), Generic, SlackResponseName(..), parseResponse, Slack(..), request')

import Network.Slack.User (User(..), userFromId)

import Data.List (find)

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (hoistEither)

import Text.Printf (printf)

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

-- | List of all channels associated with the team
channels :: Slack [Channel]
channels = mapM convertRawChannel =<< request' "channels.list"
  where
    -- Converts a ChannelRaw to a Channel by doing user id lookups
    convertRawChannel :: ChannelRaw -> Slack Channel
    convertRawChannel (ChannelRaw cid cname cuids) = do
      channelUsers <- mapM userFromId cuids
      return (Channel cid cname channelUsers)

-- | Retrieves the Channel with the corresponding name
channelFromName :: String -> Slack Channel
channelFromName cname = do
  maybeChannel <- find (\c -> channelName c == cname) <$> channels
  case maybeChannel of
   Nothing   -> Slack . hoistEither . Left . printf "Could not find channel with name: %s" $ cname
   Just channel -> Slack . hoistEither $ Right channel
