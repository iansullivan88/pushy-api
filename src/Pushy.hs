module Pushy where

import Pushy.Authentication
import qualified Pushy.Configuration as C
import Pushy.Database
import qualified Pushy.Database.Entities as PE
import qualified Pushy.Database.Types as PD
import Pushy.Types
import Pushy.Utilities
import Pushy.Web

import Control.Monad.Logger(runNoLoggingT)
import Database.Persist.MySQL
import Data.Pool
import System.Log.Logger

runPushyApi :: IO ()
runPushyApi = do 
    c <- C.readConfiguration
    C.initialiseLogging 
    pool <- runNoLoggingT $ createMySQLPool (dbInfo c) 100 
    withResource pool (\backend -> withTransaction backend $ do
        infoM logger "Initialising database"
        let defaultTeams = map (\dn -> DefaultTeam (toShortUrlPart dn) dn) (defaultTeamNames c)
        initialiseDatabase backend defaultTeams)
    let state = ApplicationState (cAuthMode c)
    let serverPort = port c
    infoM logger $ "Starting server on port " ++ show serverPort
    runPushyServer serverPort (authMode state) pool

logger :: String
logger = "Pushy.Web"
