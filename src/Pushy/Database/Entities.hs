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
import qualified Data.ByteString as B
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username T.Text maxlen=255    
    UniqueUsername username

Team
    name T.Text maxlen=30
    displayName T.Text maxlen=100
    UniqueTeamName name
    deriving Show

TeamUser
    userId UserId
    teamId TeamId
    UniqueTeamUser teamId userId

Environment
    name T.Text maxlen=30
    teamId TeamId
    UniqueEnvironmentName teamId name

ArtifactType
    name T.Text maxlen=30
    teamId TeamId
    UniqueArtifactTypeName teamId name

Artifact
    name T.Text maxlen=100
    artifactTypeId ArtifactTypeId
    UniqueTypeArtifactName artifactTypeId name

Release
    name T.Text maxlen=30
    teamId TeamId
    displayName T.Text maxlen=100
    state Int sqltype=TINYINT 
    currentEnvironment EnvironmentId Maybe
    UniqueReleaseName teamId name
    
ReleaseArtifact
    releaseId ReleaseId
    artifactId ArtifactId
    UniqueReleaseArtifact releaseId artifactId

ReleaseEnvironment
    releaseId ReleaseId
    environmentId EnvironmentId

Worker
    accessCode B.ByteString
    teamId TeamId
    UniqueAccessCode accessCode
|]
