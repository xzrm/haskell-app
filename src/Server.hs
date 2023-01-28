{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Maybe
import Database (getUpdates, getUpdatesByDate, getUpdatesByFixedYears, queryNothing)
import Helpers (strToUTC)
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

type InterestRatesAPI =
  "updates" :> QueryParam "fixedYears" Int :> QueryParam "fromDate" String :> Get '[JSON] [J.Update]

-- :<|> "updates" :> QueryParam "fromDate" String :> QueryParam "toDate" String :> Get '[JSON] [J.Update]

updatesHandler :: Maybe Int -> Maybe String -> Handler [J.Update]
updatesHandler years date
  | isJust years && isNothing date = updatesByFixedYearsHandler years
  | isNothing Nothing && isJust date = updatesByDateHandler date
  | otherwise = liftIO $ runReaderT queryNothing "rates.db"

updatesByFixedYearsHandler :: Maybe Int -> Handler [J.Update]
updatesByFixedYearsHandler years = case years of
  Just v -> liftIO $ runReaderT (getUpdatesByFixedYears v >>= \result -> return result) "rates.db"
  Nothing -> liftIO $ runReaderT queryNothing "rates.db"

updatesByDateHandler :: Maybe String -> Handler [J.Update]
updatesByDateHandler fromDate = case fromDate of
  Just date -> case strToUTC date of
    Just dateUTC -> liftIO $ runReaderT (getUpdatesByDate dateUTC dateUTC >>= \res -> return res) "rates.db"
    Nothing -> liftIO $ runReaderT queryNothing "rates.db"
  Nothing -> liftIO $ runReaderT (getUpdates >>= \result -> return result) "rates.db"

server :: Server InterestRatesAPI
server = updatesHandler

api :: Proxy InterestRatesAPI
api = Proxy

runServer :: IO ()
runServer = run 8081 . serve api $ server