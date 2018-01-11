module Web.Slack.Api.Channel
       (
        Channel(..),
        channels,
        channelFromName,
        directChannelFromUserId
       )
       where

import Web.Slack.Api.Prelude

import Web.Slack.Api.Types (SlackResponseName(..), parseStrippedPrefix, Slack(..), request', request)

import Web.Slack.Api.User (User(..), userFromId)
import qualified Data.Map as M
import Data.List (find)

-- | Represents a response from the "channels.list" command. This
-- contains a list of user IDs instead of user objects. This should be
-- converted to a Channel object
data ChannelRaw = ChannelRaw {
  _channelId :: String,
  _channelName :: String,
  _channelMembers :: [String] -- This is a list of user IDs
  } deriving (Show, Generic)

instance FromJSON ChannelRaw where
  parseJSON = parseStrippedPrefix "_channel"

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

-- | Represents a response from the "im.open" command.
-- which is just the channel id. This should be
-- converted to a DirectChannel object
data DirectChannelRaw = DirectChannelRaw {
  _directChannelId :: String
  } deriving (Show, Generic)

instance FromJSON DirectChannelRaw where
  parseJSON = parseStrippedPrefix "_directChannel"

instance SlackResponseName DirectChannelRaw where
  slackResponseName _ = "channel"

directChannelFromUserId :: String -> Slack Channel
directChannelFromUserId u = convertRawDirectChannel =<< request "im.open" args
  where
    args = M.fromList [("user", u)]

    convertRawDirectChannel :: DirectChannelRaw -> Slack Channel
    convertRawDirectChannel (DirectChannelRaw cid) =
      return $ Channel cid "" []
