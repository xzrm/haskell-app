{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use let" #-}

module JsonParser where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Cont
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Helpers (logMessage)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import QueryApi (fixed10yearsQueryParams, fixed20yearsQueryParams, mkURL)
import qualified Schemas.Schema as Schema

jsonURL10Years :: T.Text
jsonURL10Years = mkURL fixed10yearsQueryParams

jsonURL20Years :: T.Text
jsonURL20Years = mkURL fixed20yearsQueryParams

makeRequest :: Manager -> T.Text -> IO B.ByteString
makeRequest manager url = do
  liftIO $ logMessage $ T.pack "Calling " <> url
  request <- parseRequest $ T.unpack url
  responseBody <$> httpLbs request manager

decode :: B.ByteString -> Schema.Update
decode bs = case eitherDecode bs of
  Left err -> error ("Error ocurred: " <> err)
  Right rs -> rs

readJSON1 :: [T.Text] -> IO [Schema.Update]
readJSON1 urls = do
  manager <- newTlsManager
  mapConcurrently (fmap decode . makeRequest manager) urls
