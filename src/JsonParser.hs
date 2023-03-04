{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module JsonParser where

import Constants (jsonDateFormat)
import Control.Monad (MonadPlus (mzero))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON,
    Value (Object, String),
    eitherDecode,
    object,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Helpers (toUTC)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit (simpleHttp)
import QueryApi (fixed10yearsQueryParams, fixed20yearsQueryParams, mkURL)
import Text.Printf (printf)

newtype Date = Date {date :: UTCTime} deriving (Show, Generic)

data Update = Update Int UpdateB deriving (Show, Generic)

data UpdateB = UpdateB
  { products :: [Product],
    updateDateTime :: Date
  }
  deriving (Show, Generic)

newtype UpdateId = UpdateId T.Text deriving (Show, Generic)

data Product = Product
  { name :: T.Text,
    providerId :: T.Text,
    providerName :: T.Text,
    interestRate :: Double,
    form :: T.Text,
    lookupId :: T.Text,
    fixedRatePeriod :: Int
  }
  deriving (Show, Generic)

instance ToJSON Date

instance FromJSON Date where
  parseJSON (String t) = Date <$> toUTC jsonDateFormat (T.unpack t)
  parseJSON _ = mzero

-- -- These are equivalent
-- instance FromJSON Date where
--   parseJSON = withText "Date" $ \t -> do
--     d <- parseTimeM True defaultTimeLocale dateFormat (T.unpack t)
--     return (Date d)

instance FromJSON UpdateB where
  parseJSON = withObject "Update" $ \obj -> do
    products <- obj .: "producten"
    updateDateTime <- obj .: "laatstBijgewerkt"
    return (UpdateB products updateDateTime)

-- instance FromJSON Update where
--   parseJSON (Object o) =
--     Update <$> o .: "producten" <*> o .: "laatstBijgewerkt"

instance FromJSON Product where
  parseJSON (Object obj) = do
    name <- obj .: "hypotheekNaam"
    providerId <- obj .: "aanbiederId"
    providerName <- obj .: "aanbiederNaam"
    interestRate <- obj .: "rentestand"
    form <- obj .: "hypotheekVorm"
    lookupId <- obj .: "lookupId"
    fixedRatePeriod <- obj .: "rentevastePeriode"
    return
      ( Product
          name
          providerId
          providerName
          interestRate
          form
          lookupId
          fixedRatePeriod
      )
  parseJSON _ = mzero

instance ToJSON UpdateB

instance ToJSON UpdateId

instance ToJSON Update where
  toJSON (Update id (UpdateB products (Date date))) =
    object
      [ "id" .= id,
        "products" .= products,
        "update_datetime" .= date
      ]

instance ToJSON Product

-- jsonFile :: FilePath
-- jsonFile = "hypotheker.json"

jsonURL10Years :: T.Text
jsonURL10Years = mkURL fixed10yearsQueryParams

jsonURL20Years :: T.Text
jsonURL20Years = mkURL fixed20yearsQueryParams

-- getJSON :: T.Text -> IO B.ByteString
-- getJSON url = do
--   liftIO (print "before")
--   bs <- simpleHttp $ T.unpack url
--   liftIO (print bs)
--   return bs

getJsonContent :: T.Text -> IO B.ByteString
getJsonContent url = do
  manager <- newTlsManager
  liftIO $ putStrLn $ printf "Calling %s" url
  request <- parseRequest $ T.unpack url
  response <- httpLbs request manager
  return $ responseBody response

readJSON :: T.Text -> IO UpdateB
readJSON url = do
  updateJSON <- eitherDecode <$> getJsonContent url :: IO (Either String UpdateB)
  putStrLn "Reading JSON data"
  case updateJSON of
    Left err -> error err
    Right rs -> return rs

-- readJSON :: [T.Text] -> IO [UpdateB]
-- readJSON [] = pure []
-- readJSON (u : urls) = do
--   putStrLn "Reading JSON data"
--   updateJSON <- eitherDecode <$> getJsonContent u :: IO (Either String UpdateB)
--   case updateJSON of
--     Left err -> error ("Error ocurred: " <> err)
--     Right rs -> do
--       other <- readJSON urls
--       return $ rs : other