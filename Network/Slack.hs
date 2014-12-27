{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Slack where

import Network.Slack.Types

import           Control.Applicative (Applicative, (<$>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State (MonadState, StateT, evalStateT, get, modify)
import           Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)

import           Network.HTTP.Conduit (simpleHttp)

import           Data.Aeson (FromJSON(..), eitherDecode)

import qualified Data.Map as M

import           Text.Printf (printf)

type ArgName = String
type ArgValue = String
type CommandName = String
type CommandArgs = M.Map ArgName ArgValue

type Token = String
type URL = String

-- Internal state for slack commands
data SlackState = SlackState
                  {
                    _token :: Token, -- Slack API token
                    _users :: [User] -- The users in this team
                  }
                  deriving (Show)

newtype Slack a = Slack {runSlackInternal :: EitherT SlackError (StateT SlackState IO) a}
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState SlackState)

-- Given an API token and a Slack command, it executes the command in the IO monad
runSlack :: Token -> Slack a -> IO (Either SlackError a)
runSlack tok = flip evalStateT (slackAuth tok) . runEitherT . runSlackInternal . (slackInit >>)

-- Constructs an initial internal state from the given API token
slackAuth :: Token -> SlackState
slackAuth tok = SlackState tok []

-- Constructs an API request URL given the API token, command names, and command args
buildURL :: CommandName -> CommandArgs -> Slack URL
buildURL command args = do
  -- Retrieve the token from the internal state
  tokenArg <- _token <$> get
  let
    -- Insert the token into the args
    queryArgs = M.insert "token" tokenArg args
    -- Build the GET query string
    queryString :: String
    queryString = M.foldMapWithKey (printf "%s=%s&") queryArgs

    url = printf "https://slack.com/api/%s?%s" command queryString
  return url

-- Takes a API command name and the args and executes the request
request :: (SlackResponseName a, FromJSON a) => CommandName -> CommandArgs -> Slack a
request command args = do
  -- Construct the proper API url
  url <- buildURL command args
  -- Retrieve the raw JSON Data
  raw <- liftIO (simpleHttp url) 
  -- Parse it into a SlackResponse object
  resp <- Slack . hoistEither . eitherDecode $ raw
  -- Merge the Either inside the SlackResponse with the EitherT in the Slack monad stack
  Slack . hoistEither . response $ resp

-- Same as request with no command arguments
request' :: (SlackResponseName a, FromJSON a) => CommandName -> Slack a
request' command = request command M.empty

-- Internal setup. Currently it just fetches the list of users so that it can associated user ids with names
slackInit :: Slack ()
slackInit = do
  users <- request' "users.list" :: Slack [User]
  let
    updateUsers state = state {_users = users}
  -- Update internal state
  modify updateUsers

-- Gets the list of users associated with the Slack team
users :: Slack [User]
users = _users <$> get
