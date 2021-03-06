module Pushy.Utilities where

import Pushy.Types

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Maybe
import qualified Data.Text as T

toShortUrlPart :: T.Text -> T.Text
toShortUrlPart = T.pack . take 30 . mapMaybe replaceChar . T.unpack where
    replaceChar c = case c of _
                                | c == '_' || isSpace c       -> Just '-'
                                | isDigit c || isAsciiLower c -> Just c
                                | isAsciiUpper c              -> Just $ toLower c
                                | otherwise                   -> Nothing

jsonOptions :: Int -> Options
jsonOptions nPrefix   = defaultOptions { fieldLabelModifier = transformField } where
    transformField    = lowerFirst . drop nPrefix 
    lowerFirst (x:xs) = toLower x : xs

-- | Like bracket but has a separate handler the the success and
-- error cases.
bracketWithHandler :: IO a -> (a -> IO b) -> (SomeException -> a -> IO c) -> (a -> IO d) -> IO d
bracketWithHandler before onSuccess onException thing =
    mask $ \restore -> do
        a <- before
        r <- restore (thing a) `catch` (\e -> onException e a >> throwIO e)
        _ <- onSuccess a
        pure r

throwUserError :: (MonadIO m) => Int -> T.Text -> m a
throwUserError s m = liftIO $ throwIO $ UserVisibleError s m

throwApplicationError :: (MonadIO m) => Int -> T.Text -> m a
throwApplicationError s m = liftIO $ throwIO $ ApplicationError s m

throwIfNothing :: (MonadIO m, Exception e) => Maybe a -> e -> m a
throwIfNothing r = throwIfNothingM (pure r)

throwIfNothingM :: (MonadIO m, Exception e) => m (Maybe a) -> e -> m a
throwIfNothingM a e = a >>= \r -> case r of
    Nothing -> liftIO $ throwIO e
    Just v  -> pure v
