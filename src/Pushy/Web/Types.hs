{-# LANGUAGE TemplateHaskell #-}

module Pushy.Web.Types where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T

data TeamResponse = TeamResponse { teamName :: T.Text
                                 , teamDisplayName :: T.Text }
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''TeamResponse)


data ArtifactTypeResponse = ArtifactTypeResponse { artifactTypeName :: T.Text }
$(deriveJSON defaultOptions{fieldLabelModifier = drop 12} ''ArtifactTypeResponse)

data ErrorResponse = ErrorResponse { errorMessage :: T.Text }
$(deriveJSON defaultOptions ''ErrorResponse)
