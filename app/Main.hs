module Main (main) where

import Scheduler (runScheduledJobs)
import Server (runServer)

main :: IO ()
main = do
  runScheduledJobs
  putStrLn "Running Server" >> runServer
