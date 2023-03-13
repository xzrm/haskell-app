{-# LANGUAGE RecordWildCards #-}

module Converter where

import Schemas.DatabaseSchema
import qualified Schemas.Schema as Schema

fromUpdate :: Schema.Update -> UpdateEntry
fromUpdate (Schema.Update _ d) = UpdateEntry $ Schema.date d

fromProduct :: UpdateEntryId -> [Schema.Product] -> [ProductEntry]
fromProduct fKey = map (mkProductEntry fKey)
  where
    mkProductEntry fKey (Schema.Product {..}) =
      ProductEntry
        name
        providerId
        providerName
        interestRate
        form
        lookupId
        fixedRatePeriod
        fKey

toProduct :: ProductEntry -> Schema.Product
toProduct (ProductEntry name providerId providerName interestRate form lookupId fixedRatePeriod _) =
  Schema.Product
    name
    providerId
    providerName
    interestRate
    form
    lookupId
    fixedRatePeriod

toUpdate :: UpdateEntry -> [Schema.Product] -> Schema.Update
toUpdate (UpdateEntry updateDateTime) ps = Schema.Update ps (Schema.Date updateDateTime)