{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Network.Slack.Types
       (
         User(..),
         UserListResp(..),
         SlackError,
         UserId,
         UserName
       )
       where

import           GHC.Generics (Generic)

import           Data.Aeson (FromJSON(..))
import           Data.Aeson.Types (genericParseJSON, Options(..), defaultOptions)

import           Data.Char (toLower)
import           Data.List (stripPrefix)

data User = User {
  userId :: UserId,
  userName :: UserName
  } deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = uncamel "user" })
    where
      uncamel prefix str = lowercaseFirst . maybe str id . stripPrefix prefix $ str
      lowercaseFirst [] = []
      lowercaseFirst (x:xs) = toLower x : xs

data UserListResp = UserListResp {
  ok :: Bool,
  error :: Maybe SlackError,
  members :: [User]
  } deriving (Show, Generic)

instance FromJSON UserListResp

type UserId = String
type UserName = String

type SlackError = String
