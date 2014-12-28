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

import           Data.List (find)
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

-- Converts a user ID to a user object, signaling an error if there's no such user ID
userFromId :: String -> Slack User
userFromId uid = do
  maybeUser <- find (\u -> userId u == uid) <$> users :: Slack (Maybe User)
  case maybeUser of
   Nothing   -> Slack . hoistEither . Left . printf "Could not find user with id: %s" $ uid
   Just user -> Slack . hoistEither $ Right user

-- Converts a user name to a user object, signaling an error if there's no such user name
userFromName :: String -> Slack User
userFromName uname = do
  maybeUser <- find (\u -> userName u == uname) <$> users :: Slack (Maybe User)
  case maybeUser of
   Nothing   -> Slack . hoistEither . Left . printf "Could not find user with name: %s" $ uname
   Just user -> Slack . hoistEither $ Right user

-- List of all channels associated with the team
channels :: Slack [Channel]
channels = mapM convertRawChannel =<< request' "channels.list"
  where
    -- Converts a ChannelRaw to a Channel by doing user id lookups
    convertRawChannel :: ChannelRaw -> Slack Channel
    convertRawChannel (ChannelRaw cid cname cuids) = do
      channelUsers <- mapM userFromId cuids
      return (Channel cid cname channelUsers)

channelFromName :: String -> Slack Channel
channelFromName cname = do
  maybeChannel <- find (\c -> channelName c == cname) <$> channels
  case maybeChannel of
   Nothing   -> Slack . hoistEither . Left . printf "Could not find channel with name: %s" $ cname
   Just channel -> Slack . hoistEither $ Right channel
   
-- List of the past 1000 messages in the given channel
channelHistory :: Channel -> Slack [Message]
channelHistory chan = mapM convertRawMessage =<< request "channels.history" args
  where
    args = M.fromList [
      ("channel", channelId chan),
      ("count", "1000")
      ]
    convertRawMessage :: MessageRaw -> Slack Message
    convertRawMessage (MessageRaw mtype muid mtext mts) = do
      user <- userFromId muid
      return (Message mtype user mtext mts)

-- Retrieves the messages by the given user
messagesByUser :: User -> [Message] -> [Message]
messagesByUser user = filter (\m -> messageUser m == user)

brandonHistory :: Slack String
brandonHistory = do
  brandon <- userFromName "brandon"
  general <- channelFromName "general"
  messages <- messagesByUser brandon <$> channelHistory general
  return . unlines . map messageText $ messages
