module Pushy.Types where

import Control.Exception
import Database.Persist.MySQL
import qualified Data.Text as T

data PushyException = UserVisibleError Int T.Text 
                    | ApplicationError Int T.Text deriving(Show)

instance Exception PushyException

data AuthMode = NoAuthentication deriving(Show)
