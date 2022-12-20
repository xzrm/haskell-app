{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonParser where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Types.URI

data Update = Update
  { products :: [Product],
    updateDateTime :: !T.Text
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

-- instance FromJSON Update where
--   parseJSON = withObject "Update" $ \obj -> do
--     products <- obj .: "producten"
--     updateDateTime <- obj .: "laatstBijgewerkt"
--     return (Update products lastUpdateDate)

instance FromJSON Update where
  parseJSON (Object o) =
    Update <$> o .: "producten" <*> o .: "laatstBijgewerkt"

instance FromJSON Product where
  parseJSON = withObject "Product" $ \obj -> do
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
