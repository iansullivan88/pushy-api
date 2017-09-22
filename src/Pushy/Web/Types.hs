{-# LANGUAGE TemplateHaskell #-}

module Pushy.Web.Types where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T

data Team = Team { teamName :: T.Text
                 , teamDisplayName :: T.Text }
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Team)

data ErrorResponse = ErrorResponse { errorMessage :: T.Text }
$(deriveJSON defaultOptions ''ErrorResponse)
