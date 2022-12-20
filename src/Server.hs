{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class (liftIO)
import Database
import qualified JsonParser as J
import Network.Wai.Handler.Warp
import Servant

type InterestRatesAPI = "update" :> Get '[JSON] [J.Update]

createUpdatesHandler :: Handler [J.Update]
createUpdatesHandler = liftIO $ getUpdates "rates.db"

server :: Server InterestRatesAPI
server = createUpdatesHandler

api :: Proxy InterestRatesAPI
api = Proxy

runServer :: IO ()
runServer = run 8081 . serve api $ server