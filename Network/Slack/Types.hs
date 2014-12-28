module Network.Slack.Types
       (
         SlackError,
         parseStrippedPrefix,
         Token,
         Slack(..),
         SlackState(..),
         SlackResponse(..),
         SlackResponseName(..),
         ArgName,
         ArgValue,
         CommandName,
         CommandArgs,
         request,
         request',
         User(..),
         users
       )
       where

import Network.Slack.Prelude

import Data.Char (toLower)
import Data.List (stripPrefix)
import qualified Data.Map as M

import Data.Aeson (genericParseJSON)
import Data.Aeson.Types(Options(..), defaultOptions)

import           Network.HTTP.Conduit (simpleHttp)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State (MonadState, StateT)
import           Control.Monad.Trans.Either (EitherT)
import Control.Applicative(Applicative(..))

type SlackError = String

-- |A Slack Web API token
type Token = String

-- | A Slack User
data User = User {
  userId :: String,
  userName :: String
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = parseStrippedPrefix "user"

instance SlackResponseName [User] where
  slackResponseName _ = "members"

-- |Gets the list of users associated with the Slack team
users :: Slack [User]
users = _users <$> get

-- |Internal state for slack commands
data SlackState = SlackState
                  {
                    _token :: Token,  -- ^ Slack API token
                    _users :: [User]  -- ^ The users in this team. This is maintained as state to be able to reference user IDs to users
                  }
                  deriving (Show)

-- |The Slack monad. It executes commands with the context of possible failure (malformed requests, Slack is down, etc...), and some internal state
newtype Slack a = Slack {runSlackInternal :: EitherT SlackError (StateT SlackState IO) a}
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState SlackState)

-- |Parses a record from a JSON object by stripping the given prefix off of each field
parseStrippedPrefix prefix = genericParseJSON (defaultOptions {fieldLabelModifier = uncamel})
  where
    --  Removes a prefix from a string, and lowercases the first letter of the resulting string
    -- This is to turn things like userId into id
    uncamel :: String -> String
    uncamel str = lowercaseFirst . maybe str id . stripPrefix prefix $ str
    lowercaseFirst [] = []
    lowercaseFirst (x:xs) = toLower x : xs

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

type URL = String

-- |Constructs an API request URL given the API token, command names, and command args
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

-- |Takes a API command name and the args and executes the request
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

-- |Same as request with no command arguments
request' :: (SlackResponseName a, FromJSON a) => CommandName -> Slack a
request' command = request command M.empty
