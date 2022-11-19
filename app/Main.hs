{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.SimpleErrors
import Models
import Network.HTTP.Conduit (simpleHttp)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

instance FromJSON Product

instance ToJSON Product

instance ToRow Product where
  toRow (Product id hypotheekNaam aanbiederId aanbiederNaam rentestand hypotheekVorm lookupId rentevastePeriode readingId) = toRow (id, hypotheekNaam, aanbiederId, aanbiederNaam, rentestand, hypotheekVorm, lookupId, rentevastePeriode, readingId)

instance FromJSON Reading

instance ToJSON Reading

instance ToRow Reading where
  toRow (Reading id _ laatstBijgewerkt) = toRow (id, laatstBijgewerkt)

-- instance (FromField u, FromField p, FromField f, FromField l) => FromRow (UserRep (CredentialsRep u p) (NameRep f l)) where
--     fromRow = do
--         username  <- field
--         password  <- field
--         firstname <- field
--         lastname  <- field
--         return $ User (Credentials username password) (Name firstname lastname)

instance FromRow Product where
  fromRow = do
    id <- field
    n <- field
    ai <- field
    an <- field
    r <- field
    hv <- field
    l <- field
    rp <- field
    ri <- field
    return $ Product id n ai an r hv l rp ri

instance FromRow Reading where
  fromRow = do
    id <- field
    d <- field
    let p = Product Nothing "name" "abc" "name" 12.0 "form" "abc" 10 Nothing
    return $ Reading id [p] d

createProductsQuery :: Query
createProductsQuery =
  Query
    ( T.pack
        "CREATE TABLE IF NOT EXISTS products\
        \(id INTEGER PRIMARY KEY,\
        \ name TEXT, provider_id TEXT, provider TEXT,\
        \ interest_rate REAL,\
        \ morgage_form TEXT, lookup_id TEXT,\
        \ fixed_interest_period INTEGER,\
        \ reading_id INTEGER NOT NULL,\
        \ FOREIGN KEY (id) \
        \ REFERENCES readings (id));"
    )

createReadingsQuery :: Query
createReadingsQuery =
  Query
    ( T.pack
        "CREATE TABLE IF NOT EXISTS readings \
        \( id INTEGER PRIMARY KEY,\
        \ reading_date_time TEXT NOT NULL UNIQUE);"
    )

selectProductsQuery :: Query
selectProductsQuery = Query (T.pack "SELECT * FROM readings;")

openDatabase :: IO Connection
openDatabase = do
  db <- open "interest_rates.db"
  execute_ db createProductsQuery
  execute_ db createReadingsQuery
  return db

addReading :: Connection -> Reading -> IO ()
addReading conn = execute conn addReadingQuery
  where
    addReadingQuery :: Query
    addReadingQuery = Query (T.pack "INSERT INTO readings (id, reading_date_time) VALUES (?, ?);")

jsonFile :: FilePath
jsonFile = "hypotheker.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getMaxRowId :: Query
getMaxRowId = Query (T.pack "SELECT MAX(rowid) FROM readings;")

addProdcutsQuery :: Query
addProdcutsQuery =
  Query
    ( T.pack
        "INSERT INTO products\
        \(id, name, provider_id, provider,\
        \ interest_rate,\
        \ morgage_form, lookup_id,\
        \ fixed_interest_period,\
        \ reading_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);"
    )

appendForeignKey :: [Product] -> Int -> [Product]
appendForeignKey ps fk = map (f fk) ps
  where
    f fk (Product id name providerId provider rate form lookId period _) = Product id name providerId provider rate form lookId period (Just fk)

addProdcuts :: Connection -> [Product] -> Int -> IO ()
addProdcuts conn products fk = executeMany conn addProdcutsQuery $ appendForeignKey products fk

closeDatabase :: Connection -> IO ()
closeDatabase = close

insertDataToDB :: IO ()
insertDataToDB = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String Reading)
  conn <- openDatabase :: IO Connection
  case d of
    Left err -> putStrLn err
    Right rs -> do
      result <- runDBAction $ addReading conn rs
      case result of
        Left err -> print err
        Right r -> do
          rowId <- query_ conn getMaxRowId :: IO [[Int]]
          let maxRowId = head $ head rowId
          addProdcuts conn (producten rs) maxRowId
  closeDatabase conn

type ReadingsAPI = "readings" :> Get '[JSON] [Reading]

product1 :: Product
product1 = Product Nothing "name" "abc" "name" 12.0 "form" "abc" 10 Nothing

readings :: [Reading]
readings = [Reading Nothing [product1, product1] "1-1-1"]

server :: Server ReadingsAPI
server = return readings

api :: Proxy ReadingsAPI
api = Proxy

-- app1 :: Application
-- app1 = serve readingAPI server

main :: IO ()
main = do
  -- run 8081 . serve api $ server

  -- insertDataToDB

  conn <- open "interest_rates.db"
  r1 <- query_ conn "SELECT * from products" :: IO [Product]
  r2 <- query_ conn "SELECT * from readings" :: IO [Reading]
  print r1
  print r2

-- executeMany conn addProdcutsQuery $ appendForeignKey (producten ps) maxRowId