{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pushy.Database( Pushy.Database.query
                     , initialiseDatabase) where

import Pushy.Database.Types
import Pushy.Database.Entities

import qualified Data.Text as T
import Control.Monad.Reader
import Data.Maybe
import Data.Time.Clock
import Database.Persist
import Database.Persist.Sql
import System.Log.Logger

initialiseDatabase :: SqlBackend -> IO ()
initialiseDatabase = runReaderT $ runMigration migrateAll

query :: SqlBackend -> Request a -> IO a
query b r = do debugM loggerName $ "Executing request" ++ show r
               start  <- getCurrentTime
               res    <- runReaderT (executeRequest r) b
               finish <- getCurrentTime
               let elapsedMs = round $ 1000 * toRational (diffUTCTime finish start)
               liftIO $ debugM loggerName ("Finished request in " ++ show elapsedMs) 
               pure res

executeRequest :: Request a -> ReaderT SqlBackend IO a
executeRequest (GetTeamById tId) = undefined
executeRequest (InsertTeam name displayName) = insert (Team name displayName) >>= unsafeGet 


unsafeGet :: (MonadIO m, PersistRecordBackend record backend, PersistStoreRead backend) => Key record -> ReaderT backend m record
unsafeGet = fmap fromJust . get

loggerName = "Pushy.Database"
