{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module JsonParser where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Network.HTTP.Client
  ( Response (responseBody),
    httpLbs,
    parseRequest,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import QueryApi (fixed10yearsQueryParams, fixed20yearsQueryParams, mkURL)
import qualified Schemas.Schema as Schema
import Text.Printf (printf)

jsonURL10Years :: T.Text
jsonURL10Years = mkURL fixed10yearsQueryParams

jsonURL20Years :: T.Text
jsonURL20Years = mkURL fixed20yearsQueryParams

getJsonContent :: T.Text -> IO B.ByteString
getJsonContent url = do
  manager <- newTlsManager
  liftIO $ putStrLn $ printf "Calling %s" url
  request <- parseRequest $ T.unpack url
  response <- httpLbs request manager
  return $ responseBody response

readJSON :: [T.Text] -> IO [Schema.Update]
readJSON [] = pure []
readJSON (u : urls) = do
  putStrLn "Reading JSON data"
  updateJSON <- eitherDecode <$> getJsonContent u :: IO (Either String Schema.Update)
  case updateJSON of
    Left err -> error ("Error ocurred: " <> err)
    Right rs -> do
      other <- readJSON urls
      return $ rs : other