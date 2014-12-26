{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Text.Printf

type Token = String
type UserID = String
type UserName = String

type ArgName = String
type ArgValue = String
type CommandName = String
type CommandArgs = M.Map ArgName ArgValue

type URL = String

-- Internal state for slack commands
data SlackState = SlackState
                  {
                    token :: Token,                  -- Slack API token
                    userMap :: M.Map UserID UserName -- Maps user IDs to user names
                  }
                  deriving (Show)

newtype Slack a = Slack {runSlackInternal :: StateT SlackState IO a}
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState SlackState)

-- Given an API token and a Slack command, it executes the command in the IO monad
runSlack :: Token -> Slack a -> IO a
runSlack token = flip evalStateT (slackAuth token) . runSlackInternal

-- Constructs an initial internal state from the given API token
slackAuth :: Token -> SlackState
slackAuth token = SlackState token M.empty

-- Constructs an API request URL given the API token, command names, and command args
buildURL :: CommandName -> CommandArgs -> Slack URL
buildURL command args = do
  tokenArg <- token <$> get
  let queryArgs = M.insert "token" tokenArg args
      url = printf "https://slack.com/api/%s?%s" command queryString
      queryString :: String
      queryString = M.foldMapWithKey (printf "%s=%s&") queryArgs
  return url

