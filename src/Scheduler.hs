{-# LANGUAGE OverloadedStrings #-}

module Scheduler where

import Config
import Config (AppConfig (dbName))
import Database (addEntitiesJob)
import System.Cron.Schedule

runScheduledJobs :: IO ()
runScheduledJobs = do
  config <- loadConfig
  _ <- execSchedule $ addJob (addEntitiesJob (dbName config)) "* * * * *"
  return ()