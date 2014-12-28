module Network.Slack.Prelude
       (
         (<$>),
         (<*>),
         FromJSON(..),
         Value(..),
         (.:),
         eitherDecode,
         Generic,
         Text,
         unpack,
         printf,
         liftIO,
         get,
         hoistEither
         )
       where

import Control.Applicative((<$>), (<*>))

import Data.Aeson (FromJSON(..), Value(..), (.:), eitherDecode)
  
import GHC.Generics (Generic)
import Data.Text (Text, unpack)

import Text.Printf (printf)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Control.Monad.Trans.Either (hoistEither)
