{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Monad.IO.Class
import Control.Monad.Logger(runNoLoggingT)
import Data.Aeson hiding (json)
import Data.HVect
import Data.Kind
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
import Web.Routing.Combinators(PathState(Open))

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
routes = prehook userHook $ do
    get "team" $ simpleGet (PD.GetTeamsForUser <$> getContext) (fmap mapTeam)
    prehook teamUserHook $
        get (teamPath <//> "artifact") $ \_ -> simpleGet (PD.GetArtifactTypes <$> getTeamCtx) (fmap mapArtifactType)
            
        
userHook :: PushyAction ctx (Entity PE.User)
userHook = do
    authMode <- authMode <$> getState
    username <- fromMaybe (throw $ UserVisibleError 401 "Please log in") <$> authenticate authMode 
    fromMaybe (throw $ InternalError "User not found") <$> spockQuery (PD.GetUserByUsername username)

teamUserHook :: PushyAction (Entity PE.User) (Entity PE.User, Entity PE.Team)
teamUserHook = do
    user           <- getContext
    teams          <- spockQuery (PD.GetTeamsForUser user)
    (teamName:_) <- pathInfo <$> request 
    case find (\(Entity _ t) -> PE.teamName t == teamName) teams of
        Just team -> return (user, team)
        Nothing   -> throw $ UserVisibleError 403 "No access to team"

simpleGet :: (ToJSON wr) => PushyAction ctx (PD.Request r) -> (r -> wr) -> PushyAction ctx ()
simpleGet a f = do req  <- a
                   dRes <- spockQuery req
                   let wr = f dRes
                   json wr

getTeamCtx :: PushyAction (Entity PE.User, Entity PE.Team) (Entity PE.Team)
getTeamCtx = snd <$> getContext 

mapTeam :: Entity PE.Team -> PW.TeamResponse
mapTeam = mapEntity (\t -> PW.TeamResponse { PW.teamName = PE.teamName t
                                           , PW.teamDisplayName = PE.teamDisplayName t })

mapArtifactType :: Entity PE.ArtifactType -> PW.ArtifactTypeResponse
mapArtifactType = mapEntity (\a -> PW.ArtifactTypeResponse { PW.artifactTypeName = PE.artifactTypeName a })

mapEntity :: (a -> b) -> Entity a -> b
mapEntity f = f. entityVal

spockTransaction :: IO a -> PushyAction ctx a
spockTransaction a = runQuery (`withTransaction` a)

spockQuery :: PD.Request r -> PushyAction ctx r
spockQuery r = runQuery (`query` r)
              
teamPath :: Path (T.Text ': '[]) Open
teamPath = var

logger :: String
logger = "Pushy.Web"
