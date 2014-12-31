module Web.Slack
       (
         Slack(..),
         runSlack,
         module Web.Slack.User,
         module Web.Slack.Channel,
         module Web.Slack.Message
       )
       where

import Web.Slack.Channel
import Web.Slack.Message
import Web.Slack.Types
import Web.Slack.User

import Control.Monad.State (evalStateT, modify)
import Control.Monad.Trans.Either (runEitherT)

-- |Given an API token and a Slack command, it executes the command in the IO monad
runSlack :: Token -> Slack a -> IO (Either SlackError a)
runSlack tok = flip evalStateT (slackAuth tok) . runEitherT . runSlackInternal . (slackInit >>)

-- |Constructs an initial internal state from the given API token
slackAuth :: Token -> SlackState
slackAuth tok = SlackState tok []

-- |Internal setup. Currently it just fetches the list of users so that it can associated user ids with names
slackInit :: Slack ()
slackInit = do
  currentUsers <- request' "users.list" :: Slack [User]
  let
    updateUsers state = state {_users = currentUsers}
  -- Update internal state
  modify updateUsers
