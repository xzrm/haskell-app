{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Database (getUpdatesByDate, getUpdatesByFixedYears)
import Helpers (strToUTC)
import Network.Wai.Handler.Warp (run)
import Scheduler (runScheduledJobs)
import qualified Schemas.Schema as Schema
import Servant
  ( Get,
    JSON,
    Proxy (..),
    QueryParam,
    Server,
    serve,
    type (:>),
  )

type InterestRatesAPI =
  "updates" :> QueryParam "fixedYears" Int :> QueryParam "fromDate" String :> Get '[JSON] [Schema.UpdateEntity]

server :: AppConfig -> Server InterestRatesAPI
server config = updatesHandler
  where
    updatesHandler (Just years) Nothing = updatesByFixedYearsHandler years
    updatesHandler Nothing (Just date) = updatesByDateHandler date
    updatesHandler _ _ = return []

    updatesByDateHandler fromDate = case strToUTC fromDate of
      Just dateUTC -> liftIO $ runReaderT (getUpdatesByDate dateUTC dateUTC >>= return) (dbName config)
      Nothing -> liftIO $ return []

    updatesByFixedYearsHandler v = liftIO $ runReaderT (getUpdatesByFixedYears v >>= return) (dbName config)

api :: Proxy InterestRatesAPI
api = Proxy

runServer :: IO ()
runServer = do
  config <- loadConfig
  runScheduledJobs config
  run (srvPort config) . serve api $ server config
