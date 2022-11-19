{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models where

import qualified Data.Text as T
import GHC.Generics

data Reading = Reading
  { id :: Maybe Int,
    producten :: [Product],
    laatstBijgewerkt :: !T.Text
  }
  deriving (Show, Generic)

data Product = Product
  { id :: Maybe Int,
    hypotheekNaam :: !T.Text,
    aanbiederId :: !T.Text,
    aanbiederNaam :: !T.Text,
    rentestand :: !Float,
    hypotheekVorm :: !T.Text,
    lookupId :: !T.Text,
    rentevastePeriode :: !Int,
    readingId :: Maybe Int
  }
  deriving (Show, Generic)