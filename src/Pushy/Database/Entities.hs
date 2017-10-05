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

import Data.Time.Clock
import Database.Persist
import Database.Persist.TH
import qualified Data.ByteString as B
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username T.Text maxlen=255    
    UniqueUsername username
    deriving Show

Team
    name T.Text maxlen=30
    displayName T.Text maxlen=100
    UniqueTeamName name
    deriving Show

TeamUser
    teamId TeamId
    userId UserId
    UniqueTeamUser teamId userId
    deriving Show

Environment
    name T.Text maxlen=30
    teamId TeamId
    UniqueEnvironmentName teamId name
    deriving Show

ArtifactType
    name T.Text maxlen=30
    teamId TeamId
    UniqueArtifactTypeName teamId name
    deriving Show

Artifact
    name T.Text maxlen=100
    artifactTypeId ArtifactTypeId
    UniqueTypeArtifactName artifactTypeId name
    deriving Show

Release
    name T.Text maxlen=30
    teamId TeamId
    UniqueReleaseName teamId name
    deriving Show
    
ReleaseArtifact
    releaseId ReleaseId
    artifactId ArtifactId
    UniqueReleaseArtifact releaseId artifactId
    deriving Show

ReleaseEnvironment
    releaseId ReleaseId
    environmentId EnvironmentId
    state Int sqltype=TINYINT(4)
    order Int sqltype=TINYINT(4)
    deploymentStartTime UTCTime Maybe default=NULL
    deploymentEndTime UTCTime Maybe default=NULL
    UniqueReleaseEnvironment releaseId environmentId
    deriving Show

Worker
    accessCode B.ByteString
    teamId TeamId
    UniqueAccessCode accessCode
    deriving Show
|]
