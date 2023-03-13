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
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sqlite
import qualified JsonParser
import Schemas.DatabaseSchema
import qualified Schemas.Schema as Schema
import Text.Printf

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug = False
logFilter _ (LevelOther _) = False

runAction :: T.Text -> SqlPersistT (LoggingT IO) a -> IO a
runAction conn action = runStdoutLoggingT $ filterLogger logFilter $ withSqliteConn conn $ \beckend ->
  runReaderT action beckend

addEntities :: T.Text -> Schema.Update -> IO ()
addEntities dbName update = runSqlite dbName $ do
  runMigration migrateAll
  let entry = fromUpdate update
  res <- insertUnique entry
  case res of
    Nothing -> do
      liftIO $ putStrLn "nothing added to db"
      return ()
    Just entryId -> do
      let productEntries = fromProduct entryId $ Schema.products update
      mapM_ insert productEntries
      liftIO $ putStrLn "added entity to db"

addEntityGroup :: T.Text -> [Schema.Update] -> IO ()
addEntityGroup dbName updates = runAction dbName $ do
  runMigration migrateAll
  let entries = map fromUpdate updates -- check if they are the same assert
  let entry = head entries
  res <- insertUnique entry
  case res of
    Nothing -> do
      liftIO $ putStrLn "nothing added to db"
      return ()
    Just entryId -> do
      -- let productEntries = map (fromProduct entryId . products) updates
      let productEntries = updates >>= fromProduct entryId . Schema.products
      mapM_ insert productEntries
      liftIO $ putStrLn "added entity to db"

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

-- getProductEntriesByFixedYears :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend) => Int -> Key UpdateEntry -> ReaderT backend m [Entity ProductEntry]
-- getProductEntriesByFixedYears years uid = selectList [ProductEntryFixedRatePeriod ==. years, ProductEntryUpdateId ==. uid] []

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

addEntitiesJob :: T.Text -> IO ()
addEntitiesJob dbName = do
  timestamp <- getCurrentTime >>= \currentTime -> return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  putStrLn (printf "%s :: Running job: add entities to db" timestamp)
  updates <- JsonParser.readJSON [JsonParser.jsonURL10Years, JsonParser.jsonURL20Years]
  addEntityGroup dbName updates

-- addEntitiesJob :: IO ()
-- addEntitiesJob = do
--   let urls = [JsonParser.jsonURL10Years, JsonParser.jsonURL20Years]
--   timestamp <- getCurrentTime >>= \currentTime -> return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
--   putStrLn (printf "%s :: Running job: add entities to db" timestamp)
--   forM_ urls $ \url -> do
--     update <- JsonParser.readJSON url
--     print update
--     addEntities dbName update