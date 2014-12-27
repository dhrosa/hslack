{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DisambiguateRecordFields #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Map as M
import           GHC.Generics
import           Text.Printf
import           Network.HTTP.Conduit
import           Data.Char (toLower)

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
             
type Token = String
type UserId = String
type UserName = String

type ArgName = String
type ArgValue = String
type CommandName = String
type CommandArgs = M.Map ArgName ArgValue

type URL = String
type SlackError = String

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
runSlack token = flip evalStateT (slackAuth token) . runEitherT . runSlackInternal

-- Constructs an initial internal state from the given API token
slackAuth :: Token -> SlackState
slackAuth token = SlackState token M.empty

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
