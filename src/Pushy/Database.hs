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
                             traverse_ createTeam ts
                             query b $ UpsertUser "anonymous"
                             where
    createTeam (DefaultTeam n dn) = query b $ UpsertTeam n dn

withTransaction :: SqlBackend -> IO a -> IO a
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
executeRequest (UpsertTeam name displayName) = void $ upsert (Team name displayName) [TeamDisplayName =. displayName] 
executeRequest (UpsertUser username) = void $ upsert (User username) [UserUsername =. username] 

unsafeGet :: (MonadIO m, PersistRecordBackend record backend, PersistStoreRead backend) => Key record -> ReaderT backend m record
unsafeGet = fmap fromJust . get

loggerName = "Pushy.Database"
