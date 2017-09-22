{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pushy.Database( Pushy.Database.query
                     , initialiseDatabase
                     , withTransaction) where

import Pushy.Types
import Pushy.Database.Types
import Pushy.Database.Entities

import qualified Data.Text as T
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import Data.Time.Clock
import Database.Persist
import Database.Persist.Sql
import System.Log.Logger

initialiseDatabase :: SqlBackend -> [DefaultTeam] -> IO ()
initialiseDatabase b ts = do runReaderT (runMigration migrateAll) b
                             query b $ CreateDefaultTeamsAndUser ts

withTransaction :: (MonadIO m) => SqlBackend -> IO a -> m a
withTransaction b a = runReaderT (transactionUndo *> liftIO a <* transactionSave) b

query :: SqlBackend -> Request a -> IO a
query b r = do debugM loggerName $ "Executing request" ++ show r
               start  <- getCurrentTime
               res    <- runReaderT (executeRequest r) b
               finish <- getCurrentTime
               let elapsedMs = round $ 1000 * toRational (diffUTCTime finish start)
               liftIO $ debugM loggerName ("Finished request in " ++ show elapsedMs) 
               pure res

executeRequest :: Request a -> ReaderT SqlBackend IO a
executeRequest (CreateDefaultTeamsAndUser ts) = do
    let username = "anonymous"
    (Entity uId _) <- upsert (User username) [UserUsername =. username]
    teams          <- traverse (\(DefaultTeam n dn) -> upsert (Team n dn) [TeamDisplayName =. dn]) ts
    let teamIds = map entityKey teams
    deleteWhere [TeamUserUserId ==. uId, TeamUserTeamId /<-. teamIds]
    traverse_ (\tId -> upsert (TeamUser tId uId) []) teamIds
executeRequest (GetUserByUsername n) = getBy (UniqueUsername n)
executeRequest (GetTeamsForUser (Entity uId _)) = do
    tu <- selectList [TeamUserUserId ==. uId] []
    let teamIds = fmap (teamUserTeamId  . entityVal)  tu
    selectList [TeamId <-. teamIds] []

loggerName = "Pushy.Database"
