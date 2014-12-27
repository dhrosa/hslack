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
         User(..)
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

-- Represents a Slack channel
data Channel = Channel {
  channelId :: String,
  channelName :: String,
  channelMembers :: [User]
  } deriving (Show, Generic)

instance FromJSON Channel where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = uncamel "channel"})

instance SlackResponseName [Channel] where
  slackResponseName _ = "channels"
