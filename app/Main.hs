{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database
import qualified Server as S
import System.Cron.Schedule

main :: IO ()
main = do
  _ <- execSchedule $ addJob addEntitiesJob "* * * * *"
  putStrLn "Running Server" >> S.runServer

myJob :: IO ()
myJob = do
  print ("dummy job" :: String)
