{-# LANGUAGE OverloadedStrings #-}

module QueryApi where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types

baseURL :: T.Text
baseURL = "https://api2.hypotheker.nl/v2/interestrates/hypotheekaanbieders"

data QueryParam
  = Form {name :: T.Text, value :: Maybe T.Text}
  | IsNew {name :: T.Text, value :: Maybe T.Text}
  | BaseRate {name :: T.Text, value :: Maybe T.Text}
  | FixedPeriod {name :: T.Text, value :: Maybe T.Text}

-- values
annualForm :: QueryParam
annualForm = Form "HypotheekVorm" (Just "Annuiteitenhypotheek")

notNew :: QueryParam
notNew = IsNew "IsNieuwbouw" (Just "false")

zeroBaseRate :: QueryParam
zeroBaseRate = BaseRate "RenteBasis" (Just "0")

fixedPeriod10years :: QueryParam
fixedPeriod10years = FixedPeriod "RentevastePeriode" (Just "10")

fixedPeriod20years :: QueryParam
fixedPeriod20years = FixedPeriod "RentevastePeriode" (Just "20")

-- queries
fixed20yearsQueryParams :: [QueryParam]
fixed20yearsQueryParams =
  [ annualForm,
    notNew,
    zeroBaseRate,
    fixedPeriod20years
  ]

fixed10yearsQueryParams :: [QueryParam]
fixed10yearsQueryParams =
  [ annualForm,
    notNew,
    zeroBaseRate,
    fixedPeriod10years
  ]

queryToTuple :: QueryParam -> (T.Text, Maybe T.Text)
queryToTuple (Form n v) = (n, v)
queryToTuple (IsNew n v) = (n, v)
queryToTuple (BaseRate n v) = (n, v)
queryToTuple (FixedPeriod n v) = (n, v)

encode :: (T.Text, Maybe T.Text) -> QueryItem
encode (t1, t2) = (T.encodeUtf8 t1, fmap T.encodeUtf8 t2)

mkURL :: [QueryParam] -> T.Text
mkURL qs = baseURL <> T.decodeUtf8 (renderQuery True $ map (encode . queryToTuple) qs)
