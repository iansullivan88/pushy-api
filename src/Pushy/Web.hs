{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Pushy.Web(runPushyApi) where

import Pushy.Authentication
import Pushy.Database
import qualified Pushy.Database.Entities as PE
import qualified Pushy.Database.Types as PD
import qualified Pushy.Configuration as C
import Pushy.Types
import Pushy.Utilities
import qualified Pushy.Web.Types as PW

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Logger(runNoLoggingT)
import Data.HVect
import Data.Maybe
import Data.Pool
import Data.List
import Database.Persist.MySQL hiding(get)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Network.HTTP.Types
import Network.Wai hiding(Application)
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
                 let state = ApplicationState (cAuthMode c)
                 sc <- defaultSpockCfg () (PCPool pool) state
                 let sc' = sc { spc_csrfProtection = False }
                 let serverPort = port c
                 infoM logger $ "Starting server on port " ++ show serverPort
                 runSpockNoBanner serverPort (spock sc' routes)

routes :: SpockM SqlBackend () ApplicationState ()
routes = subcomponent "api" $
    prehook teamUserHook $ error "not implemented"
        

teamUserHook :: ActionCtxT () Application (Entity PE.User, Entity PE.Team)
teamUserHook = do
    authMode <- authMode <$> getState
    username <- fromMaybe (throw $ UserVisibleError 401 "Please log in") <$> authenticate authMode 
    user     <- fromMaybe (throw $ InternalError "User not found") <$> spockQuery (PD.GetUserByUsername username)
    teams    <- spockQuery (PD.GetTeamsForUser user)
    (_:teamName:_) <- pathInfo <$> request 
    case find (\(Entity _ t) -> PE.teamName t == teamName) teams of
        Just team -> return (user, team)
        Nothing   -> throw $ UserVisibleError 403 "No access to team"

spockQuery r = runQuery (`query` r)
              
logger :: String
logger = "Pushy.Web"
