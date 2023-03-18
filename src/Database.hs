{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Database where

import Config
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
  ( LogLevel (..),
    LoggingT,
    filterLogger,
    runStdoutLoggingT,
  )
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Converter (fromProduct, fromUpdate, toProduct, toUpdate)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist.Sqlite
import Helpers (logMessage)
import qualified JsonParser
import Schemas.DatabaseSchema
import qualified Schemas.Schema as Schema

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug = False
logFilter _ (LevelOther _) = False

runAction :: T.Text -> SqlPersistT (LoggingT IO) a -> IO a
runAction conn action = runStdoutLoggingT $ filterLogger logFilter $ withSqliteConn conn $ \beckend ->
  runReaderT action beckend

addEntityGroup :: AppConfig -> [Schema.Update] -> IO ()
addEntityGroup (AppConfig _ dbName _ envType) updates = runAction dbName $ do
  case envType of
    Dev -> liftIO (logMessage "Running migration") >> runMigration migrateAll
    _ -> return ()
  let entries = map fromUpdate updates -- check if they are the same assert
  let entry = head entries
  res <- insertUnique entry
  case res of
    Nothing -> do
      liftIO $ logMessage "nothing added to db"
      return ()
    Just entryId -> do
      -- let productEntries = map (fromProduct entryId . products) updates
      let productEntries = updates >>= fromProduct entryId . Schema.products
      mapM_ insert productEntries
      liftIO $ logMessage "added entity to db"

-- getProducts :: T.Text -> IO [ProductEntry]
-- getProducts dbName = runSqlite dbName $ do
--   ps :: [Entity ProductEntry] <- selectList [] []
--   return $ map entityVal ps

keyToInt :: Key UpdateEntry -> Int
keyToInt = fromIntegral . fromSqlKey

getUpdates :: ReaderT DbConnection IO [Schema.UpdateEntity]
getUpdates = do
  conn <- ask
  runSqlite conn $ do
    updates :: [Entity UpdateEntry] <- selectList [] []
    forM updates $ \(Entity updateId update) -> do
      prodEntities <- selectList [ProductEntryUpdateId ==. updateId] []
      return $ Schema.UpdateEntity (keyToInt updateId) (mkUpdate update prodEntities)

getUpdatesByFixedYears :: Int -> ReaderT DbConnection IO [Schema.UpdateEntity]
getUpdatesByFixedYears years = do
  conn <- ask
  runSqlite conn $ do
    updates :: [Entity UpdateEntry] <- selectList [] []
    forM updates $ \(Entity updateId update) -> do
      prodEntities <- selectList [ProductEntryFixedRatePeriod ==. years, ProductEntryUpdateId ==. updateId] []
      return $ Schema.UpdateEntity (keyToInt updateId) (mkUpdate update prodEntities)

getUpdatesByDate :: UTCTime -> UTCTime -> ReaderT DbConnection IO [Schema.UpdateEntity]
getUpdatesByDate fromDate _ = do
  conn <- ask
  runSqlite conn $ do
    updates :: [Entity UpdateEntry] <- selectList [UpdateEntryUpdateDateTime >=. fromDate] [Asc UpdateEntryUpdateDateTime]
    forM updates $ \(Entity updateId update) -> do
      prodEntities <- selectList [ProductEntryUpdateId ==. updateId] []
      return $ Schema.UpdateEntity (keyToInt updateId) (mkUpdate update prodEntities)

queryNothing :: ReaderT DbConnection IO [Schema.UpdateEntity]
queryNothing = return []

mkUpdate :: UpdateEntry -> [Entity ProductEntry] -> Schema.Update
mkUpdate u ps = toUpdate u $ map (toProduct . entityVal) ps

addEntitiesJob :: AppConfig -> IO ()
addEntitiesJob appConfig = do
  logMessage "Running job: add entities to db"
  updates <- JsonParser.readJSON [JsonParser.jsonURL10Years, JsonParser.jsonURL20Years]
  addEntityGroup appConfig updates