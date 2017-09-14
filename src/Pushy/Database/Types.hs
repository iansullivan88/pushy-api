{-# LANGUAGE GADTs #-} 
{-# LANGUAGE StandaloneDeriving #-}

module Pushy.Database.Types where

import Pushy.Database.Entities

import qualified Data.Text as T
import Database.Persist.Types

data Request a where
    UpsertTeam :: T.Text -> T.Text -> Request ()
    UpsertUser :: T.Text -> Request ()

deriving instance Show (Request a)
