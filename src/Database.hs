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

module Database where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import JsonParser as J

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
    updateDateTime T.Text
    UniqueUpdateDateTime updateDateTime
    deriving Show
|]

addEntities :: T.Text -> J.Update -> IO ()
addEntities dbName update = runSqlite dbName $ do
  runMigration migrateAll
  let updateEntry = fromUpdate update
  res <- insertUnique updateEntry
  case res of
    Nothing -> do
      liftIO $ putStrLn "nothing added to db"
      return ()
    Just entryId -> do
      let productEntries = fromProduct entryId $ products update
      mapM_ insert productEntries

fromUpdate :: J.Update -> UpdateEntry
fromUpdate (J.Update _ date) = UpdateEntry date

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
toUpdate (UpdateEntry updateDateTime) ps = J.Update ps updateDateTime

-- getProducts :: T.Text -> IO [ProductEntry]
-- getProducts dbName = runSqlite dbName $ do
--   ps :: [Entity ProductEntry] <- selectList [] []
--   return $ map entityVal ps

getUpdates :: T.Text -> IO [J.Update]
getUpdates dbName = runSqlite dbName $ do
  updates :: [Entity UpdateEntry] <- selectList [] []
  forM updates $ \(Entity updateId update) -> do
    prodEntryEntities <- selectList [ProductEntryUpdateId ==. updateId] []
    let products = map (toProduct . entityVal) prodEntryEntities
    return $ toUpdate update products

addEntitiesJob :: IO ()
addEntitiesJob = do
  putStrLn "Running job: add entities to db"
  update <- J.readJSON
  addEntities "rates.db" update