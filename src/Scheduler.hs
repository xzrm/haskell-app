{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Scheduler where

import Config (AppConfig (dbName), jobInterval)
import Database (addEntitiesJob)
import System.Cron.Schedule

runScheduledJobs :: AppConfig -> IO ()
runScheduledJobs config = do
  _ <- execSchedule $ addJob (addEntitiesJob config) (jobInterval config)
  return ()