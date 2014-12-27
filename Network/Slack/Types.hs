{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Slack.Types
       (
         SlackError,
         SlackResponse(..),
         SlackResponseName(..),
         User(..),
         ChannelRaw(..),
         Channel(..),
         MessageRaw(..),
         Message(..)
       )
       where

import Debug.Trace
import GHC.Generics (Generic)

import Control.Applicative (Applicative, (<$>), (<*>))

import Data.Aeson (FromJSON(..), (.:), (.:?))
import Data.Aeson.Types (Value(..), genericParseJSON, Options(..), defaultOptions)
import Data.Text (Text)

import Data.Char (toLower)
import Data.List (stripPrefix)

type SlackError = String

-- Represents the response the Slack API returns
 -- If the Slack API has an "ok" member, the 
data SlackResponse a = SlackResponse { response :: Either SlackError a }
                       deriving (Show)

-- Maps response types to the name of the key in the Slack API
-- For example, the "users.list" command returns the list of users in a key labeled "members"
class SlackResponseName a where
  slackResponseName :: a -> Text

instance (FromJSON a, SlackResponseName a) => FromJSON (SlackResponse a) where
  parseJSON (Object v) = do
    ok <- v .: "ok"
    if ok
      -- If success, get the name of the key to parse, and return the parsed object
      then SlackResponse . Right <$> v .: slackResponseName (undefined :: a)
      -- Else get the error message
      else SlackResponse . Left <$> v .: "error"

-- Removes a prefix from a string, and lowercases the first letter of the resulting string
-- This is to turn things like userId into id
uncamel :: String -> String -> String
uncamel prefix str = lowercaseFirst . maybe str id . stripPrefix prefix $ str
  where
    lowercaseFirst [] = []
    lowercaseFirst (x:xs) = toLower x : xs

-- A Slack User
data User = User {
  userId :: String,
  userName :: String
  } deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = uncamel "user" })

instance SlackResponseName [User] where
  slackResponseName _ = "members"

-- Represents a response from the "channels.list" command. This
-- contains a list of user IDs instead of user objects. This should be
-- converted to a Channel object
data ChannelRaw = ChannelRaw {
  channelRawId :: String,
  channelRawName :: String,
  channelRawMembers :: [String] -- This is a list of user IDs
  } deriving (Show, Generic)

instance FromJSON ChannelRaw where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = uncamel "channelRaw"})

instance SlackResponseName [ChannelRaw] where
  slackResponseName _ = "channels"

-- A more useable version of ChannelRaw
data Channel = Channel {
  channelId :: String,
  channelName :: String,
  channelMembers :: [User]
} deriving (Show)

-- A message sent on a channel. Message can also mean things like user joined or a message was edited
data MessageRaw = MessageRaw {
  messageRawType :: String,
  messageRawUser :: String, -- user ID
  messageRawText :: String
} deriving (Show, Generic)

instance FromJSON MessageRaw where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = uncamel "messageRaw"})

instance SlackResponseName [MessageRaw] where 
  slackResponseName _ = "messages"

-- A nicer version of MessageRaw, with the user id converted to a User
data Message = Message {
  messageType :: String,
  messageUser :: User,
  messageText :: String
  } deriving (Show)
