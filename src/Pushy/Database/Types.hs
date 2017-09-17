{-# LANGUAGE GADTs #-} 
{-# LANGUAGE StandaloneDeriving #-}

module Pushy.Database.Types where

import Pushy.Database.Entities
import Pushy.Types

import qualified Data.Text as T
import Database.Persist.Types

data Request a where
    CreateDefaultTeamsAndUser :: [DefaultTeam] -> Request ()
    GetUserByUsername :: T.Text -> Request (Maybe (Entity User))
    GetTeamByName :: T.Text -> Request (Maybe (Entity Team))
    IsUserInTeam :: Entity User -> Entity Team -> Request Bool  
deriving instance Show (Request a)
