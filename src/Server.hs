{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Maybe (isJust, isNothing)
import Database (getUpdatesByDate, getUpdatesByFixedYears)
import Helpers (strToUTC)
import Network.Wai.Handler.Warp (run)
import qualified Schemas.Schema as Schema
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
  "updates" :> QueryParam "fixedYears" Int :> QueryParam "fromDate" String :> Get '[JSON] [Schema.UpdateEntity]

-- :<|> "updates" :> QueryParam "fromDate" String :> QueryParam "toDate" String :> Get '[JSON] [J.Update]

updatesHandler :: Maybe Int -> Maybe String -> Handler [Schema.UpdateEntity]
updatesHandler years date
  | isJust years && isNothing date = updatesByFixedYearsHandler years
  | isNothing Nothing && isJust date = updatesByDateHandler date
  | otherwise = liftIO $ return []

updatesByFixedYearsHandler :: Maybe Int -> Handler [Schema.UpdateEntity]
updatesByFixedYearsHandler years = case years of
  Just v -> do
    config <- liftIO loadConfig
    liftIO $ runReaderT (getUpdatesByFixedYears v >>= return) (dbName config)
  Nothing -> liftIO $ return []

updatesByDateHandler :: Maybe String -> Handler [Schema.UpdateEntity]
updatesByDateHandler fromDate = case fromDate of
  Just date -> case strToUTC date of
    Just dateUTC -> do
      config <- liftIO loadConfig
      liftIO $ runReaderT (getUpdatesByDate dateUTC dateUTC >>= return) (dbName config)
    Nothing -> liftIO $ return []
  Nothing -> liftIO $ return []

server :: Server InterestRatesAPI
server = updatesHandler

api :: Proxy InterestRatesAPI
api = Proxy

runServer :: IO ()
runServer = do
  config <- loadConfig
  run (srvPort config) . serve api $ server
