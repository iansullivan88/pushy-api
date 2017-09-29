{-# LANGUAGE TemplateHaskell #-}

module Pushy.Web.Types where

import Pushy.Utilities

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import qualified Data.Text as T

data TeamResponse = TeamResponse { teamName :: T.Text
                                 , teamDisplayName :: T.Text }
$(deriveJSON (jsonOptions 4) ''TeamResponse)


data ArtifactTypeResponse = ArtifactTypeResponse { artifactTypeName :: T.Text }
$(deriveJSON (jsonOptions 12) ''ArtifactTypeResponse)

data ErrorResponse = ErrorResponse { errorMessage :: T.Text }
$(deriveJSON defaultOptions ''ErrorResponse)
