{-# LANGUAGE OverloadedStrings #-}

module Pushy.Authentication(authenticate) where

import Control.Monad
import Data.Foldable
import Database.Persist.Sql
import Pushy.Database
import Pushy.Database.Types
import Pushy.Types
import Web.Spock

import qualified Data.Text as T

authenticate :: AuthMode -> SpockAction SqlBackend () ApplicationState T.Text
authenticate NoAuthentication = pure "anonymous"


