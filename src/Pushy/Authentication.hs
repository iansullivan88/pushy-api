{-# LANGUAGE OverloadedStrings #-}

module Pushy.Authentication(getAuthenticatedUsername) where

import Pushy.Database
import Pushy.Database.Types
import Pushy.Types

import qualified Data.Text as T

getAuthenticatedUsername :: AuthMode -> Maybe T.Text
getAuthenticatedUsername NoAuthentication = Just "anonymous"
