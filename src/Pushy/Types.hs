module Pushy.Types where

import Database.Persist.MySQL
import qualified Data.Text as T
import Web.Spock

data AuthMode = NoAuthentication deriving(Show)

data DefaultTeam = DefaultTeam { dName :: T.Text
                               , dDisplayName :: T.Text } deriving(Show)

data PushyConfig = PushyConfig { port :: Int
                               , cAuthMode :: AuthMode
                               , dbInfo :: MySQLConnectInfo
                               , defaultTeamNames :: [T.Text] }

data ApplicationState = ApplicationState { authMode :: AuthMode }
