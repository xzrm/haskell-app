{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text
import qualified Data.Text as T
import Database.SQLite.Simple
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

newtype GroupId' a = GroupId a

type GroupId = GroupId' Int

deriving instance Generic GroupId

deriving instance Show GroupId

data Group' i n = Group
  { groupId :: i,
    name :: n
  }
  deriving (Show, Generic)

type Group = Group' GroupId Text

instance FromJSON GroupId

instance ToJSON GroupId

instance FromJSON Group

instance ToJSON Group

-- | Type of each JSON entry in record syntax.
data Person = Person
  { id_ :: Maybe Int,
    firstName :: !Text,
    lastName :: !Text,
    age :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON Person

instance ToJSON Person

instance ToRow Person where
  toRow (Person id_ firstName lastName age) = toRow (id_, firstName, lastName, age)

-- A query for creating a table in the database. Does nothing if the table exists already.
-- createQuery :: Query
-- createQuery = Query (T.pack "CREATE TABLE IF NOT EXISTS persons (id INTEGER PRIMARY KEY, firstName TEXT, lastName TEXT, age INTEGER);")

-- openDatabase :: IO Connection
-- openDatabase = do
--   db <- open "persons.db"
--   execute_ db createQuery
--   return db

-- addQuery :: Query
-- addQuery = Query (T.pack "INSERT INTO persons (id, firstName, lastName, age) VALUES (?, ?, ?, ?);")

-- jsonFile :: FilePath
-- jsonFile = "persons.json"

jsonFile :: FilePath
jsonFile = "groups.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
  -- d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
  d <- (eitherDecode <$> getJSON) :: IO (Either String Group)
  -- db <- openDatabase
  case d of
    Left err -> putStrLn err
    -- Right ps -> executeMany db addQuery ps
    Right group -> print group