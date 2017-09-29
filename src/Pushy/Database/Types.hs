{-# LANGUAGE GADTs #-} 
{-# LANGUAGE StandaloneDeriving #-}

module Pushy.Database.Types where

import Pushy.Database.Entities
import Pushy.Types

import qualified Data.Text as T
import Database.Persist.Types

data Request a where
    CreateDefaultTeamsAndUser :: [(T.Text, T.Text)] -> Request ()
    GetUserByUsername :: T.Text -> Request (Maybe (Entity User))
    GetTeamsForUser :: Entity User -> Request [Entity Team]
    GetArtifactTypes :: Entity Team -> Request [Entity ArtifactType]
    AddArtifactType :: Entity Team -> T.Text -> Request ()
deriving instance Show (Request a)
