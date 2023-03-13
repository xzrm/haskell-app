{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Schemas.Schema where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON,
    Value (Object, String),
    object,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Helpers (jsonDateFormat, toUTC)

newtype Date = Date {date :: UTCTime} deriving (Show, Generic)

data UpdateEntity = UpdateEntity Int Update deriving (Show, Generic)

data Update = Update
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

instance ToJSON UpdateId

instance ToJSON UpdateEntity where
  toJSON (UpdateEntity id (Update products (Date date))) =
    object
      [ "id" .= id,
        "products" .= products,
        "update_datetime" .= date
      ]

instance ToJSON Product