{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Database (getUpdates, getUpdatesByFixedYears)
import qualified JsonParser as J
import Network.Wai.Handler.Warp (run)
import Servant
  ( Get,
    Handler,
    JSON,
    Proxy (..),
    QueryParam,
    Server,
    serve,
    type (:>),
  )

type InterestRatesAPI = "updates" :> QueryParam "fixedYears" Int :> Get '[JSON] [J.Update]

createUpdatesHandler :: Maybe Int -> Handler [J.Update]
createUpdatesHandler years = case years of
  Just v -> liftIO $ runReaderT (getUpdatesByFixedYears v >>= \result -> return result) "rates.db"
  Nothing -> liftIO $ runReaderT (getUpdates >>= \result -> return result) "rates.db"

server :: Server InterestRatesAPI
server = createUpdatesHandler

api :: Proxy InterestRatesAPI
api = Proxy

runServer :: IO ()
runServer = run 8081 . serve api $ server