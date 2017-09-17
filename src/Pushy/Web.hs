{-# LANGUAGE OverloadedStrings #-}

module Pushy.Web(runPushyApi) where

import Pushy.Authentication
import Pushy.Database
import qualified Pushy.Configuration as C
import Pushy.Types
import Pushy.Utilities

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Logger(runNoLoggingT)
import Data.Pool
import Database.Persist.MySQL hiding(get)
import qualified Data.Text as T
import qualified Data.ByteString as B
import System.Log.Logger
import Web.Spock
import Web.Spock.Config

runPushyApi :: IO ()
runPushyApi = do c <- C.readConfiguration
                 C.initialiseLogging 
                 pool <- runNoLoggingT $ createMySQLPool (dbInfo c) 100 
                 withResource pool (\backend -> withTransaction backend $ do
                    infoM logger "Initialising database"
                    let defaultTeams = map (\dn -> DefaultTeam (toShortUrlPart dn) dn) (defaultTeamNames c)
                    initialiseDatabase backend defaultTeams)
                 sc <- defaultSpockCfg () (PCPool pool) ()
                 let sc' = sc { spc_csrfProtection = False }
                 let serverPort = port c
                 let state = ApplicationState (cAuthMode c)
                 sc <- defaultSpockCfg () (PCPool pool) state
                 infoM logger $ "Starting server on port " ++ show serverPort
                 runSpockNoBanner serverPort  (spock sc' routes)

routes :: SpockM SqlBackend () () ()
routes = do get root $ text "Pushy!"

logger :: String
logger = "Pushy.Web"


