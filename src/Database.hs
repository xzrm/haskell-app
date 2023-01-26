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

module Database where

import Control.Monad
-- import Control.Monad.Trans.Reader
-- import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Data.Text as T
import Data.Time (UTCTime)
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
    PersistStoreWrite (insert),
    PersistUniqueWrite (insertUnique),
    selectList,
    (==.),
  )
import Database.Persist.Sqlite
  ( BackendKey (SqlBackendKey),
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
    Update (Update, products),
    jsonURL10Years,
    jsonURL20Years,
    readJSON,
  )

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

addEntities :: T.Text -> J.Update -> IO ()
addEntities dbName update = runSqlite dbName $ do
  runMigration migrateAll
  let entry = fromUpdate update
  res <- insertUnique entry
  case res of
    Nothing -> do
      liftIO $ putStrLn "nothing added to db"
      return ()
    Just entryId -> do
      let productEntries = fromProduct entryId $ products update
      mapM_ insert productEntries

addEntityGroup :: T.Text -> [J.Update] -> IO ()
addEntityGroup dbName updates = runSqlite dbName $ do
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
      let productEntries = updates >>= fromProduct entryId . products
      mapM_ insert productEntries

fromUpdate :: J.Update -> UpdateEntry
fromUpdate (J.Update _ d) = UpdateEntry $ date d

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

toUpdate :: UpdateEntry -> [J.Product] -> J.Update
toUpdate (UpdateEntry updateDateTime) ps = J.Update ps (Date updateDateTime)

-- getProducts :: T.Text -> IO [ProductEntry]
-- getProducts dbName = runSqlite dbName $ do
--   ps :: [Entity ProductEntry] <- selectList [] []
--   return $ map entityVal ps

type DbConnection = T.Text

getUpdates :: ReaderT DbConnection IO [J.Update]
getUpdates = do
  conn <- ask
  runSqlite conn $ do
    updates :: [Entity UpdateEntry] <- selectList [] []
    forM updates $ \(Entity updateId update) -> do
      prodEntities <- selectList [ProductEntryUpdateId ==. updateId] []
      return $ mkUpdate update prodEntities

-- getProductEntriesByFixedYears :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend) => Int -> Key UpdateEntry -> ReaderT backend m [Entity ProductEntry]
-- getProductEntriesByFixedYears years uid = selectList [ProductEntryFixedRatePeriod ==. years, ProductEntryUpdateId ==. uid] []

getUpdatesByFixedYears :: Int -> ReaderT DbConnection IO [J.Update]
getUpdatesByFixedYears years = do
  conn <- ask
  runSqlite conn $ do
    updates :: [Entity UpdateEntry] <- selectList [] []
    forM updates $ \(Entity updateId update) -> do
      prodEntities <- selectList [ProductEntryFixedRatePeriod ==. years, ProductEntryUpdateId ==. updateId] []
      return $ mkUpdate update prodEntities

mkUpdate :: UpdateEntry -> [Entity ProductEntry] -> Update
mkUpdate u ps = toUpdate u $ map (toProduct . entityVal) ps

addEntitiesJob :: IO ()
addEntitiesJob = do
  putStrLn "Running job: add entities to db"
  updates <- J.readJSON [jsonURL10Years, jsonURL20Years]
  addEntityGroup "rates.db" updates