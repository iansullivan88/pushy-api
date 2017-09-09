module Pushy.Utilities where

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
