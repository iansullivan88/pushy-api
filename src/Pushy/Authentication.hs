module Pushy.Authentication(initialiseAuthentication) where

import Control.Monad
import Data.Foldable
import Database.Persist.Sql
import Pushy.Database
import Pushy.Database.Types
import Pushy.Types

import qualified Data.Text as T

initialiseAuthentication :: SqlBackend -> AuthenticationMode -> IO ()
initialiseAuthentication b (NoAuthentication ts) = traverse_ (createDefaultTeam b) ts

createDefaultTeam :: SqlBackend -> DefaultTeam -> IO ()
createDefaultTeam b (DefaultTeam n dn) = void $ query b $ InsertTeam n dn
