{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Database where

import Constants (DbConnection)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock
import Database.Persist
  ( Entity (Entity, entityVal),
    FieldDef
      ( fieldAttrs,
        fieldCascade,
        fieldComments,
        fieldDB,
        fieldGenerated,
        fieldHaskell,
        fieldIsImplicitIdColumn,
        fieldReference,
        fieldSqlType,
        fieldStrict,
        fieldType
      ),
    Key,
    PersistStoreWrite (insert),
    PersistUniqueWrite (insertUnique),
    SelectOpt (Asc),
    selectList,
    (==.),
    (>=.),
  )
import Database.Persist.Sqlite
  ( BackendKey (SqlBackendKey),
    fromSqlKey,
    runMigration,
    runSqlite,
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import JsonParser as J
  ( Date (Date, date),
    Product (..),
    Update (Update),
    UpdateB (..),
    jsonURL10Years,
    jsonURL20Years,
    readJSON,
  )
import Text.Printf

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
ProductEntry
    name T.Text
    providerId T.Text
    providerName T.Text
    interestRate Double
    form T.Text
    lookupId T.Text
    fixedRatePeriod Int
    updateId UpdateEntryId 
    deriving Show
UpdateEntry
    updateDateTime UTCTime
    UniqueUpdateDateTime updateDateTime
    deriving Show
|]

addEntities :: T.Text -> J.UpdateB -> IO ()
addEntities dbName update = runSqlite dbName $ do
  -- runMigration migrateAll
  let entry = fromUpdate update
  res <- insertUnique entry
  case res of
    Nothing -> do
      liftIO $ putStrLn "nothing added to db"
      return ()
    Just entryId -> do
      let productEntries = fromProduct entryId $ products update
      mapM_ insert productEntries
      liftIO $ putStrLn "added entity to db"

addEntityGroup :: T.Text -> [J.UpdateB] -> IO ()
addEntityGroup dbName updates = runSqlite dbName $ do
  -- runMigration migrateAll
  let entries = map fromUpdate updates -- check if they are the same assert
  let entry = head entries
  res <- insertUnique entry
  case res of
    Nothing -> do
      liftIO $ putStrLn "nothing added to db"
      return ()
    Just entryId -> do
      -- let productEntries = map (fromProduct entryId . products) updates
      let productEntries = updates >>= fromProduct entryId . products
      mapM_ insert productEntries
      liftIO $ putStrLn "added entity to db"

fromUpdate :: J.UpdateB -> UpdateEntry
fromUpdate (J.UpdateB _ d) = UpdateEntry $ date d

fromProduct :: UpdateEntryId -> [J.Product] -> [ProductEntry]
fromProduct fKey = map (mkProductEntry fKey)
  where
    mkProductEntry fKey (J.Product {..}) =
      ProductEntry
        name
        providerId
        providerName
        interestRate
        form
        lookupId
        fixedRatePeriod
        fKey

toProduct :: ProductEntry -> J.Product
toProduct (ProductEntry name providerId providerName interestRate form lookupId fixedRatePeriod _) =
  J.Product
    name
    providerId
    providerName
    interestRate
    form
    lookupId
    fixedRatePeriod

toUpdate :: UpdateEntry -> [J.Product] -> J.UpdateB
toUpdate (UpdateEntry updateDateTime) ps = J.UpdateB ps (Date updateDateTime)

-- getProducts :: T.Text -> IO [ProductEntry]
-- getProducts dbName = runSqlite dbName $ do
--   ps :: [Entity ProductEntry] <- selectList [] []
--   return $ map entityVal ps

keyToInt :: Key UpdateEntry -> Int
keyToInt = fromIntegral . fromSqlKey

-- getUpdates1 :: ReaderT DbConnection (Writer String) [Entity UpdateEntry]
-- getUpdates1 = do
--   conn <- ask
--   updates:: [Entity UpdateEntry] <- lift $ runSqlite conn $ do
--     selectList [] []
--   return updates

getUpdates :: ReaderT DbConnection IO [J.Update]
getUpdates = do
  conn <- ask
  runSqlite conn $ do
    updates :: [Entity UpdateEntry] <- selectList [] []
    forM updates $ \(Entity updateId update) -> do
      prodEntities <- selectList [ProductEntryUpdateId ==. updateId] []
      return $ J.Update (keyToInt updateId) (mkUpdate update prodEntities)

-- getProductEntriesByFixedYears :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend) => Int -> Key UpdateEntry -> ReaderT backend m [Entity ProductEntry]
-- getProductEntriesByFixedYears years uid = selectList [ProductEntryFixedRatePeriod ==. years, ProductEntryUpdateId ==. uid] []

getUpdatesByFixedYears :: Int -> ReaderT DbConnection IO [J.Update]
getUpdatesByFixedYears years = do
  conn <- ask
  runSqlite conn $ do
    updates :: [Entity UpdateEntry] <- selectList [] []
    forM updates $ \(Entity updateId update) -> do
      prodEntities <- selectList [ProductEntryFixedRatePeriod ==. years, ProductEntryUpdateId ==. updateId] []
      return $ J.Update (keyToInt updateId) (mkUpdate update prodEntities)

getUpdatesByDate :: UTCTime -> UTCTime -> ReaderT DbConnection IO [J.Update]
getUpdatesByDate fromDate _ = do
  conn <- ask
  runSqlite conn $ do
    updates :: [Entity UpdateEntry] <- selectList [UpdateEntryUpdateDateTime >=. fromDate] [Asc UpdateEntryUpdateDateTime]
    forM updates $ \(Entity updateId update) -> do
      prodEntities <- selectList [ProductEntryUpdateId ==. updateId] []
      return $ J.Update (keyToInt updateId) (mkUpdate update prodEntities)

queryNothing :: ReaderT DbConnection IO [J.Update]
queryNothing = return []

mkUpdate :: UpdateEntry -> [Entity ProductEntry] -> J.UpdateB
mkUpdate u ps = toUpdate u $ map (toProduct . entityVal) ps

-- addEntitiesJob :: IO ()
-- addEntitiesJob = do
--   putStrLn "Running job: add entities to db"
--   -- updates <- J.readJSON [jsonURL10Years, jsonURL20Years]
--   updates <- J.readJSON [jsonURL10Years]
--   addEntityGroup "rates.db" updates

addEntitiesJob :: IO ()
addEntitiesJob = do
  let urls = [jsonURL10Years, jsonURL20Years]
  -- currentTime <- getCurrentTime
  -- let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  -- putStrLn (printf "%s :: Running job: add entities to db" timestamp)
  timestamp <- getCurrentTime >>= \currentTime -> return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  putStrLn (printf "%s :: Running job: add entities to db" timestamp)
  forM_ urls $ \url -> do
    update <- J.readJSON url
    addEntities "rates.db" update