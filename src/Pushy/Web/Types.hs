{-# LANGUAGE TemplateHaskell #-}

module Pushy.Web.Types where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T

data Team = Team { name :: T.Text
                 , displayName :: T.Text }
$(deriveJSON defaultOptions ''Team)

data ErrorResponse = ErrorResponse { errorMessage :: T.Text }
$(deriveJSON defaultOptions ''ErrorResponse)
