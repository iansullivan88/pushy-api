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

authenticate :: AuthMode -> ActionCtxT a Application (Maybe T.Text)
authenticate NoAuthentication = pure $ Just "anonymous"
