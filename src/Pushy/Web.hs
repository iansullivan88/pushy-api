{-# LANGUAGE OverloadedStrings #-}

module Pushy.Web(runPushyApi) where

import Pushy.Authentication
import qualified Pushy.Configuration as C

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Text
import Web.Spock
import Web.Spock.Config

runPushyApi :: IO ()
runPushyApi = do c  <- C.readConfiguration
                 sc <- defaultSpockCfg () PCNoDatabase ()
                 let sc' = sc { spc_csrfProtection = False } 
                 runSpockNoBanner (C.port c) (spock sc' routes)

routes :: SpockM () () () ()
routes = do get root $ text "Pushy!"



