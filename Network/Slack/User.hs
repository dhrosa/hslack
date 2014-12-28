module Network.Slack.User
       (
         User(..),
         users,
         userFromId,
         userFromName
       )
       where

import Network.Slack.Types (User(..), Slack(..), users)

import Data.List (find)
import Text.Printf (printf)

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (hoistEither)

-- |Converts a user ID to a user object, signaling an error if there's no such user ID
userFromId :: String -> Slack User
userFromId uid = do
  maybeUser <- find (\u -> userId u == uid) <$> users :: Slack (Maybe User)
  case maybeUser of
   Nothing   -> Slack . hoistEither . Left . printf "Could not find user with id: %s" $ uid
   Just user -> Slack . hoistEither $ Right user

-- | Converts a user name to a user object, signaling an error if there's no such user name
userFromName :: String -> Slack User
userFromName uname = do
  maybeUser <- find (\u -> userName u == uname) <$> users :: Slack (Maybe User)
  case maybeUser of
   Nothing   -> Slack . hoistEither . Left . printf "Could not find user with name: %s" $ uname
   Just user -> Slack . hoistEither $ Right user

