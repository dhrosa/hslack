module Web.Slack.Api
       (
         Slack(..),
         runSlack,
         request,
         module Web.Slack.Api.User,
         module Web.Slack.Api.Channel,
         module Web.Slack.Api.Message
       )
       where

import Web.Slack.Api.Channel
import Web.Slack.Api.Message
import Web.Slack.Api.Types
import Web.Slack.Api.User

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
slackInit = return ()
--  currentUsers <- request' "users.list" :: Slack [User]
-- Commented out the below to make it faster since we don't need user lists
{-  let
    updateUsers state = state {_users = currentUsers}
  -- Update internal state
  modify updateUsers
-}
