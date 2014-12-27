{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Slack where

import Network.Slack.Types

import           Control.Applicative (Applicative, (<$>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State (MonadState, StateT, evalStateT, get, modify)
import           Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)

import           Network.HTTP.Conduit (simpleHttp)

import           Data.Aeson (eitherDecode)

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
  tokenArg <- _token <$> get
  let queryArgs = M.insert "token" tokenArg args
      queryString :: String
      queryString = M.foldMapWithKey (printf "%s=%s&") queryArgs
      url = printf "https://slack.com/api/%s?%s" command queryString
  return url

-- Internal setup. Currently it fetches the list of users so that it can associated user ids with names
slackInit :: Slack ()
slackInit = do
  url <- buildURL "users.list" M.empty
  raw <- liftIO (simpleHttp url)
  userListResp <- Slack . hoistEither . eitherDecode $ raw
  let
    updateUsers state = state {_users = members userListResp}
  -- Update internal state
  modify updateUsers

-- Gets the list of users associated with the Slack team
users :: Slack [User]
users = _users <$> get
