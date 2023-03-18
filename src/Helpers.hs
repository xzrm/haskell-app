{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Helpers where

import qualified Data.Text as T
import Data.Time (ParseTime, UTCTime, formatTime, parseTimeM)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Text.Printf

dateFormat :: String
dateFormat = "%Y-%-m-%-d"

jsonDateFormat :: String
jsonDateFormat = "%FT%T%Q"

toUTC :: (MonadFail m, ParseTime t) => String -> String -> m t
toUTC = parseTimeM True defaultTimeLocale

strToUTC :: String -> Maybe UTCTime
strToUTC = toUTC dateFormat

textToUTC :: T.Text -> Maybe UTCTime
textToUTC t = toUTC jsonDateFormat (T.unpack t)

logMessage :: T.Text -> IO ()
logMessage msg = do
  timestamp <- getCurrentTime >>= \currentTime -> return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  putStrLn (printf "%s :: %s" timestamp msg)
