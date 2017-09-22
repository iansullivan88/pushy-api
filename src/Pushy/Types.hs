module Pushy.Types where

import Control.Exception
import Database.Persist.MySQL
import qualified Data.Text as T
import Web.Spock

type PushyAction ctx = ActionCtxT ctx Application
type Application = WebStateM SqlBackend () ApplicationState

data PushyException = UserVisibleError Int T.Text 
                    | InternalError T.Text deriving(Show)
instance Exception PushyException

data AuthMode = NoAuthentication deriving(Show)

data DefaultTeam = DefaultTeam { dName :: T.Text
                               , dDisplayName :: T.Text } deriving(Show)

data PushyConfig = PushyConfig { port :: Int
                               , cAuthMode :: AuthMode
                               , dbInfo :: MySQLConnectInfo
                               , defaultTeamNames :: [T.Text] }

data ApplicationState = ApplicationState { authMode :: AuthMode }
