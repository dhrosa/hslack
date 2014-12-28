{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Slack.Types
       (
         FromJSON(..),
         Generic,
         SlackError,
         CommandName,
         CommandArgs,
         SlackResponse(..),
         SlackResponseName(..),
         parseResponse
       )
       where

import GHC.Generics (Generic)

import Control.Applicative ((<$>))

import Data.Aeson (FromJSON(..), (.:))
import Data.Aeson.Types (Value(..), genericParseJSON, Options(..), defaultOptions)

import Data.Text (Text)

import Data.Char (toLower)
import Data.List (stripPrefix)

import qualified Data.Map as M

type SlackError = String

type ArgName = String
type ArgValue = String
type CommandName = String
type CommandArgs = M.Map ArgName ArgValue

-- | Represents the response the Slack API returns
data SlackResponse a = SlackResponse { response :: Either SlackError a }
                       deriving (Show)

-- | Maps response types to the name of the key in the Slack API
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

  parseJSON _ = fail "Expected an Object."

parseResponse prefix = genericParseJSON (defaultOptions {fieldLabelModifier = uncamel})
  where
    --  Removes a prefix from a string, and lowercases the first letter of the resulting string
    -- This is to turn things like userId into id
    uncamel :: String -> String
    uncamel str = lowercaseFirst . maybe str id . stripPrefix prefix $ str
    lowercaseFirst [] = []
    lowercaseFirst (x:xs) = toLower x : xs
