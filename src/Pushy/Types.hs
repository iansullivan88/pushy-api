module Pushy.Types where

import Database.Persist.MySQL
import qualified Data.Text as T

data AuthenticationMode = NoAuthentication [DefaultTeam] deriving(Show)

data DefaultTeam = DefaultTeam { dName :: T.Text
                               , dDisplayName :: T.Text } deriving(Show)

data PushyConfig = PushyConfig { port :: Int
                               , authMode :: AuthenticationMode
                               , dbInfo :: MySQLConnectInfo }

                                
