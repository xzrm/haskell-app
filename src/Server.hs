{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Constants (dbName)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Maybe (isJust, isNothing)
import Database (getUpdatesByDate, getUpdatesByFixedYears, queryNothing)
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
  | otherwise = liftIO $ runReaderT queryNothing dbName

updatesByFixedYearsHandler :: Maybe Int -> Handler [J.Update]
updatesByFixedYearsHandler years = case years of
  Just v -> liftIO $ runReaderT (getUpdatesByFixedYears v >>= \result -> return result) dbName
  Nothing -> liftIO $ runReaderT queryNothing dbName

updatesByDateHandler :: Maybe String -> Handler [J.Update]
updatesByDateHandler fromDate = case fromDate of
  Just date -> case strToUTC date of
    Just dateUTC -> liftIO $ runReaderT (getUpdatesByDate dateUTC dateUTC >>= \res -> return res) dbName
    Nothing -> liftIO $ runReaderT queryNothing dbName
  Nothing -> liftIO $ runReaderT queryNothing dbName

server :: Server InterestRatesAPI
server = updatesHandler

api :: Proxy InterestRatesAPI
api = Proxy

runServer :: IO ()
runServer = run 8081 . serve api $ server