{-# LANGUAGE GADTs #-} 
{-# LANGUAGE StandaloneDeriving #-}

module Pushy.Database.Types where

import Pushy.Database.Entities

import qualified Data.Text as T
import Database.Persist.Types

data Request a where
    InsertTeam :: T.Text -> T.Text -> Request Team
    UpsertTeam :: T.Text -> T.Text -> Request (Entity Team)
    GetTeamById :: Int -> Request Team

deriving instance Show (Request a)
