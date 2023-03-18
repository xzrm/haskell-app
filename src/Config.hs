{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Config where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Yaml
import Helpers (logMessage)
import System.Environment (lookupEnv)

-- Define a data type to represent your configuration
data Config = Config
  { port :: Int,
    dev :: EnvVars,
    pro :: EnvVars
  }
  deriving (Show, Eq)

data AppConfig = AppConfig
  { srvPort :: Int,
    dbName :: DbConnection,
    jobInterval :: T.Text,
    envType :: EnvType
  }
  deriving (Show, Eq)

data EnvVars = EnvVars
  { dbConn :: DbConnection,
    interval :: T.Text
  }
  deriving (Show, Eq)

data EnvType = Dev | Pro deriving (Show, Eq)

type DbConnection = T.Text

instance FromJSON Config where
  parseJSON (Object m) = Config <$> m .: "port" <*> m .: "dev" <*> m .: "pro"
  parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON EnvVars where
  parseJSON (Object m) = EnvVars <$> m .: "dbConn" <*> m .: "interval"
  parseJSON x = fail ("not an object: " ++ show x)

parseEnv :: String -> EnvType
parseEnv "pro" = Pro
parseEnv _ = Dev

getEnvVars :: EnvType -> Config -> EnvVars
getEnvVars envType cfg = case envType of
  Pro -> pro cfg
  _ -> dev cfg

mkAppConfig :: Config -> EnvType -> AppConfig
mkAppConfig cfg@(Config port _ _) envType = AppConfig port dbStr interval envType
  where
    EnvVars dbStr interval = getEnvVars envType cfg

-- Define a function to load the configuration
loadConfig :: IO AppConfig
loadConfig = do
  config <- either (error . show) id <$> decodeFileEither "config.yml"
  logMessage $ T.pack ("Loaded config: " ++ show config)
  envType <- fromMaybe "dev" <$> lookupEnv "ENV_TYPE"
  let appCfg = mkAppConfig config (parseEnv envType)
  logMessage $ T.pack ("Loaded app config: " ++ show appCfg)
  return appCfg
