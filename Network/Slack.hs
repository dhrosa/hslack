{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Slack where

import Network.Slack.Types

import           Control.Applicative (Applicative, (<$>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State (MonadState, StateT, evalStateT, get)
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
                    token :: Token,                  -- Slack API token
                    userMap :: M.Map UserId UserName -- Maps user IDs to user names
                  }
                  deriving (Show)

newtype Slack a = Slack {runSlackInternal :: EitherT SlackError (StateT SlackState IO) a}
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState SlackState)

-- Given an API token and a Slack command, it executes the command in the IO monad
runSlack :: Token -> Slack a -> IO (Either SlackError a)
runSlack tok = flip evalStateT (slackAuth tok) . runEitherT . runSlackInternal

-- Constructs an initial internal state from the given API token
slackAuth :: Token -> SlackState
slackAuth tok = SlackState tok M.empty

-- Constructs an API request URL given the API token, command names, and command args
buildURL :: CommandName -> CommandArgs -> Slack URL
buildURL command args = do
  tokenArg <- token <$> get
  let queryArgs = M.insert "token" tokenArg args
      queryString :: String
      queryString = M.foldMapWithKey (printf "%s=%s&") queryArgs
      url = printf "https://slack.com/api/%s?%s" command queryString
  return url

userList :: Slack UserListResp
userList = do
  url <- buildURL "users.list" M.empty
  raw <- liftIO (simpleHttp url)
  Slack . hoistEither . eitherDecode $ raw
