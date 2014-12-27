{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Slack.Types
       (
         User(..),
         SlackError,
         SlackResponse(..),
         SlackResponseName(..),
         UserId,
         UserName
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

-- Removes a prefix from a string, and lowercases the first letter of the resulting string
-- This is to turn things like userId into id
uncamel :: String -> String -> String
uncamel prefix str = lowercaseFirst . maybe str id . stripPrefix prefix $ str
  where
    lowercaseFirst [] = []
    lowercaseFirst (x:xs) = toLower x : xs

data User = User {
  userId :: UserId,
  userName :: UserName
  } deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = uncamel "user" })

data SlackResponse a = SlackResponse { response :: Either String a }
                       deriving (Show)

class SlackResponseName a where
  slackResponseName :: a -> Text

instance SlackResponseName [User] where
  slackResponseName _ = "members"

instance (FromJSON a, SlackResponseName a) => FromJSON (SlackResponse a) where
  parseJSON (Object v) = do
    ok <- v .: "ok"
    if ok
      -- If success, get the name of the key to parse, and return the parsed object
      then SlackResponse . Right <$> v .: slackResponseName (undefined :: a)
      -- Else get the error message
      else SlackResponse . Left <$> v .: "error"

  parseJSON _ = trace "HELLO" (return undefined)

type UserId = String
type UserName = String

type SlackError = String

data Channel = Channel {
  channelId :: String,
  channelName :: String,
  channelMembers :: [User]
  } deriving (Show, Generic)

instance FromJSON Channel where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = uncamel "channel"})
