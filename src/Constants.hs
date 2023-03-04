{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Constants where

import qualified Data.Text as T

type DbConnection = T.Text

dbName :: DbConnection
dbName = T.pack "rates.db"

dateFormat :: String
dateFormat = "%Y-%-m-%-d"

jsonDateFormat :: String
jsonDateFormat = "%FT%T%Q"