{-# LANGUAGE OverloadedStrings #-}

module Pushy.Configuration(readConfiguration
                         , initialiseLogging) where

import Pushy.Types
import Pushy.Utilities

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Configurator
import Data.Configurator.Types
import Data.List
import Data.Maybe
import Database.Persist.MySQL
import qualified Data.Text as T
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler(setFormatter)
import System.Log.Handler.Simple (streamHandler, GenericHandler)
import System.IO(stdout,stderr)


readConfiguration :: IO PushyConfig
readConfiguration = do c      <- load [Required "pushy-api.config"]
                       dbHost <- lookupDefault "localhost" c "databaseHost"
                       dbPort <- fromInteger <$> lookupDefault 3306 c "databasePort"
                       dbUser <- require c "databaseUser"
                       dbPassword <- require c "databasePassword"
                       let dbInfo'  = setMySQLConnectInfoPort dbPort $ mkMySQLConnectInfo dbHost dbUser dbPassword "pushy"
                       PushyConfig <$> lookupDefault 80 c "port"
                                   <*> (parseAuth =<< require c "authenticationMode")
                                   <*> pure dbInfo'
                                   <*> lookupDefault [] c "defaultTeams"

parseAuth :: String -> IO AuthMode
parseAuth "none" = pure NoAuthentication
parseAuth _      = throw $ KeyError "authenticationMode"

initialiseLogging :: IO ()
initialiseLogging = do outHandler   <- configureHandler <$> streamHandler stdout DEBUG
                       errorHandler <- configureHandler <$> streamHandler stderr ERROR
                       updateGlobalLogger rootLoggerName (setLevel DEBUG)
                       updateGlobalLogger rootLoggerName (setHandlers [outHandler, errorHandler]) 
    where configureHandler h = setFormatter h (simpleLogFormatter "[$time $loggername $tid $prio] $msg")
    
    
