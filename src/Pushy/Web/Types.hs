{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Pushy.Web.Types where

import Pushy.Utilities

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import qualified Data.Text as T

-- ** Requests

data ArtifactRequest = ArtifactRequest { artifactTypeName :: T.Text }
$(deriveJSON (jsonOptions 0) ''ArtifactRequest)

-- ** Responses

data TeamResponse = TeamResponse { name :: T.Text
                                 , displayName :: T.Text }
$(deriveJSON (jsonOptions 0) ''TeamResponse)


data ArtifactTypeResponse = ArtifactTypeResponse { name :: T.Text }
$(deriveJSON (jsonOptions 0) ''ArtifactTypeResponse)

data ErrorResponse = ErrorResponse { errorMessage :: T.Text }
$(deriveJSON defaultOptions ''ErrorResponse)
