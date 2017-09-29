{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Pushy.Web(runPushyServer) where

import Pushy.Authentication
import qualified Pushy.Database
import qualified Pushy.Database.Entities as PE
import qualified Pushy.Database.Types as PD
import Pushy.Types
import Pushy.Utilities
import Pushy.Web.CommonRoutes
import qualified Pushy.Web.Types as PW

import Control.Applicative
import Control.Exception hiding(Handler)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson hiding (json)
import qualified Data.ByteString as B
import Data.ByteString.Conversion.From
import qualified Data.ByteString.Lazy as LB
import Data.Kind
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Pool
import Data.Predicate
import qualified Data.Text as T
import Data.Text.Encoding
import Database.Persist.Sql
import qualified Network.Wai as W
import Network.Wai.Handler.Warp
import Network.Wai.Routing.Request
import Network.Wai.Predicate.Error(Error)
import Network.Wai.Predicate.Request
import qualified Network.Wai.Routing.Route as R
import Network.HTTP.Types
import Network.HTTP.Types.Method
import System.Log.Logger

type BasicHandler = (W.Request, [(B.ByteString, B.ByteString)]) -> IO W.Response

type Handler auth = ReaderT (RequestContext auth) IO

data RequestContext auth = RequestContext { request :: W.Request
                                          , auth    :: auth
                                          , conn    :: SqlBackend
                                          , capts   :: [(B.ByteString, B.ByteString)] }                                              

runPushyServer :: Int -> AuthMode -> Pool SqlBackend -> IO ()
runPushyServer port authMode pool = do
    let waiRoutes = register registerWaiRoutes (routes authMode pool)
    let app = R.route $ R.prepare $ waiRoutes >> todo
    let settings = setPort port $ setOnExceptionResponse exceptionResponse defaultSettings 
    runSettings settings app

routes :: AuthMode -> Pool SqlBackend -> Routes (Method, B.ByteString) BasicHandler ()
routes authMode pool =
    withHandler (globalHandler pool) $ 
        withHandler (userHandler authMode) $ do
            get "teams" getTeams
            withHandler userTeamHandler $ do
                get  ":teamName/artifactType" getArtifactTypes
                post ":teamName/artifactType/:name" addArtifactType
    where
    get = route' methodGet
    post = route' methodPost
    route' method path = route (method, path)

registerWaiRoutes :: (Method, B.ByteString) -> BasicHandler -> R.Routes b IO ()
registerWaiRoutes (m, p) h = R.addRoute m p (R.continue h) inputPredicate where
    inputPredicate req = Okay 0 (getRequest req, captures req)

userHandler :: AuthMode -> Handler (Entity PE.User) a -> Handler () a
userHandler authMode h = do
    let un = fromMaybe (throw $ UserVisibleError 401 "Please log in") (getAuthenticatedUsername authMode)
    mUser <- query $ PD.GetUserByUsername un
    let user = fromMaybe (throw $ ApplicationError ("Could not find user with name " <> un)) mUser
    withReaderT (\r -> r { auth = user }) h

userTeamHandler :: Handler (Entity PE.User, Entity PE.Team) r -> Handler (Entity PE.User) r
userTeamHandler h = do
    user     <- asks auth
    teamName <- capture "teamName"
    teams    <- query (PD.GetTeamsForUser user)
    case find (\(Entity _ t) -> PE.teamName t == teamName) teams of
        Just team -> withReaderT (\r -> r { auth = (user, team) }) h 
        Nothing   -> throw $ UserVisibleError 403 "No access to team"
                

globalHandler :: Pool SqlBackend -> Handler () W.Response -> BasicHandler
globalHandler p h (r,cs) = withResource p (\b ->
    -- Handle any application errors inside withResource - these shouldn't
    -- result in pooled connections getting destroyed
    catch (Pushy.Database.withTransaction b $ runReaderT h (ctx b)) handleException) where
    handleException = pure . exceptionResponse . toException :: PushyException -> IO W.Response
    ctx b = RequestContext r () b cs
            
exceptionResponse :: SomeException -> W.Response
exceptionResponse e = respond st "Error" (PW.ErrorResponse mess) where
    (st, mess) = case fromException e of
                      Just (UserVisibleError s m) -> (s, "Error")
                      otherwise                   -> (500, "An unexpected error occurred")

respondOk :: (ToJSON r) => r -> W.Response
respondOk = respond 200 "OK"

respond :: (ToJSON r) => Int -> B.ByteString -> r -> W.Response
respond s m o = W.responseLBS (mkStatus s m) [(hContentType, "application/json")] (encode o)

getTeams :: Handler (Entity PE.User) W.Response
getTeams = simpleHandler (pure . PD.GetTeamsForUser) (fmap mapTeam) where
   mapTeam (Entity _ t) = PW.TeamResponse { PW.teamName = PE.teamName t
                                          , PW.teamDisplayName = PE.teamDisplayName t }

getArtifactTypes :: Handler (Entity PE.User, Entity PE.Team) W.Response
getArtifactTypes = simpleHandler (pure . PD.GetArtifactTypes . snd) (fmap mapArtifactType) where
  mapArtifactType (Entity _ a) = PW.ArtifactTypeResponse { PW.artifactTypeName = PE.artifactTypeName a }

addArtifactType :: Handler (Entity PE.User, Entity PE.Team) W.Response
addArtifactType = simpleHandler (\(_,t) -> capture "name" >>= pure . PD.AddArtifactType t) id

simpleHandler :: (ToJSON rw) => (auth -> Handler auth (PD.Request r)) -> (r -> rw) -> Handler auth W.Response
simpleHandler fReq fRes = do a   <- asks auth
                             req <- fReq a
                             res <- query req
                             pure $ respondOk $ fRes res


capture :: (FromByteString r) => B.ByteString -> Handler auth r
capture key = asks capts >>= \cs ->
         let v = fromMaybe err (lookup key cs)
         in pure $ fromMaybe err (fromByteString v) where
    err = throw $ ApplicationError $ "Couldn't read capture: " <> decodeUtf8 key

query :: PD.Request r -> Handler auth r
query req = asks conn >>= \b ->
    liftIO $ Pushy.Database.query b req

logger :: String
logger = "Pushy.Web"
