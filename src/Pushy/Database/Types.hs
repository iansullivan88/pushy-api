{-# LANGUAGE GADTs #-} 
{-# LANGUAGE StandaloneDeriving #-}

module Pushy.Database.Types where

import Pushy.Database.Entities

import qualified Data.Text as T

data Request a where
    InsertTeam  :: T.Text -> T.Text -> Request Team
    GetTeamById :: Int -> Request Team

deriving instance Show (Request a)
