{-# LANGUAGE OverloadedStrings #-}

module Pushy where

import Pushy.Authentication
import Pushy.Database
import qualified Pushy.Database.Entities as PE
import qualified Pushy.Database.Types as PD
import Pushy.Types
import Pushy.Utilities
import Pushy.Web

import Control.Exception
import Control.Monad.Logger(runNoLoggingT)
import Data.Configurator
import Data.Configurator.Types
import Data.Pool
import Database.Persist.MySQL
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler(setFormatter)
import System.Log.Handler.Simple (streamHandler, GenericHandler)
import System.IO(stdout,stderr)

runPushyApi :: IO ()
runPushyApi = do 
    c <- load [Required "pushy-api.config"]
    initialiseLogging 
    port <- lookupDefault 80 c "port"
    authMode <- require c "authenticationMode" >>= parseAuth
    pool <- createConnectionPool c
    withResource pool (\b -> do
        initialiseDatabase b
        withTransaction b $ initialiseData c b)
    infoM logger $ "Starting server on port " ++ show port
    runPushyServer port authMode pool

createConnectionPool :: Config -> IO (Pool SqlBackend)
createConnectionPool c = do
    dbHost <- lookupDefault "localhost" c "databaseHost"
    dbPort <- fromInteger <$> lookupDefault 3306 c "databasePort"
    dbUser <- require c "databaseUser"
    dbPassword <- require c "databasePassword"
    let dbInfo = setMySQLConnectInfoPort dbPort $ mkMySQLConnectInfo dbHost dbUser dbPassword "pushy"
    runNoLoggingT $ createMySQLPool dbInfo 100

initialiseData :: Config -> SqlBackend -> IO ()
initialiseData c b = do
    names <- lookupDefault [] c "defaultTeams"
    let ts = map (\dn -> (toShortUrlPart dn, dn)) names
    query b $ PD.CreateDefaultTeamsAndUser ts

parseAuth :: String -> IO AuthMode
parseAuth "none" = pure NoAuthentication
parseAuth _      = throw $ KeyError "authenticationMode"

initialiseLogging :: IO ()
initialiseLogging = do outHandler   <- configureHandler <$> streamHandler stdout DEBUG
                       updateGlobalLogger rootLoggerName (setLevel DEBUG)
                       updateGlobalLogger rootLoggerName (setHandlers [outHandler]) 
    where configureHandler h = setFormatter h (simpleLogFormatter "[$time $loggername $tid $prio] $msg")
    

logger :: String
logger = "Pushy"
