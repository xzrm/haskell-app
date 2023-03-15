module Main (main) where

import Server (runServer)

main :: IO ()
main = do
  putStrLn "Running Server" >> runServer
