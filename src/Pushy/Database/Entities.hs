{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Pushy.Database.Entities where

import Database.Persist
import Database.Persist.TH
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Team
    name T.Text maxlen=30
    displayName T.Text maxlen=100
    UniqueTeamName name
    deriving Show

Environment
    name T.Text maxlen=30
    displayName T.Text maxlen=100
    teamId TeamId
    UniqueEnvironment teamId name

DeploymentConfiguration
    name T.Text maxlen=30
    displayName T.Text maxlen=100
    teamId TeamId
    UniqueDeploymentConfiguration teamId name
    deriving Show

|]
