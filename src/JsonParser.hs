{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module JsonParser where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Types.URI

newtype Date = Date {date :: UTCTime} deriving (Show, Generic)

data Update = Update
  { products :: [Product],
    updateDateTime :: Date
  }
  deriving (Show, Generic)

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

dateFormat :: String
dateFormat = "%FT%T%Q"

instance ToJSON Date

instance FromJSON Date where
  parseJSON (String t) = Date <$> parseTimeM True defaultTimeLocale dateFormat (T.unpack t)
  parseJSON _ = mzero

-- -- These are equivalent
-- instance FromJSON Date where
--   parseJSON = withText "Date" $ \t -> do
--     d <- parseTimeM True defaultTimeLocale dateFormat (T.unpack t)
--     return (Date d)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \obj -> do
    products <- obj .: "producten"
    updateDateTime <- obj .: "laatstBijgewerkt"
    return (Update products updateDateTime)

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

instance ToJSON Update

instance ToJSON Product

jsonFile :: FilePath
jsonFile = "hypotheker.json"

baseURL :: String
baseURL = "https://api2.hypotheker.nl/v2/interestrates/hypotheekaanbieders"

queryParams :: [(BS.ByteString, Maybe BS.ByteString)]
queryParams =
  [ ("HypotheekVorm", Just "Annuiteitenhypotheek"),
    ("IsNieuwbouw", Just "false"),
    ("RenteBasis", Just "0"),
    ("RentevastePeriode", Just "10")
  ]

jsonURL :: String
jsonURL = baseURL <> (T.unpack . T.decodeUtf8 $ renderQuery True queryParams)

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

-- getJSON :: IO B.ByteString
-- getJSON = B.readFile jsonFile

readJSON :: IO Update
readJSON = do
  update <- (eitherDecode <$> getJSON) :: IO (Either String Update)
  case update of
    Left err -> error err
    Right rs -> return rs
