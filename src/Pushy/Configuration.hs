{-# LANGUAGE OverloadedStrings #-}

module Pushy.Configuration(PushyConfig
                         , port
                         , authenticationMode
                         , databaseHost
                         , databaseUser
                         , databasePassword
                         , databasePort
                         , readConfiguration) where

import Pushy.Authentication

import Control.Exception
import Data.Configurator
import Data.Configurator.Types
import Data.Text

data PushyConfig = PushyConfig { port :: Int
                               , authenticationMode :: AuthenticationMode
                               , databaseHost :: String
                               , databaseUser :: String
                               , databasePassword :: String
                               , databasePort :: Int }

readConfiguration :: IO PushyConfig
readConfiguration = do c <- load [Required "pushy-api.config"]
                       PushyConfig <$> lookupDefault 80 c "port"
                                   <*> (parseAuth =<< require c "authenticationMode")
                                   <*> lookupDefault "localhost" c "databaseHost"
                                   <*> require c "databaseUser"
                                   <*> require c "databasePassword"
                                   <*> lookupDefault 3306   c "databasePort" where
    parseAuth m = case toLower m of
                      "none"    -> return NoAuthentication
                      otherwise -> throw $ KeyError "authenticationMode"

