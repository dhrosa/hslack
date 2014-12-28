module Network.Slack.Monad
       where

import Network.Slack.Types

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State (MonadState, StateT)
import           Control.Monad.Trans.Either (EitherT)
import Control.Applicative(Applicative(..))

-- |A Slack Web API token
type Token = String
type URL = String

-- | A Slack User
data User = User {
  userId :: String,
  userName :: String
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = parseResponse "user"

instance SlackResponseName [User] where
  slackResponseName _ = "members"

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
