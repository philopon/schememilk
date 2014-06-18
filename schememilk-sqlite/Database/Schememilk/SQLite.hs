{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Schememilk.SQLite
    (SQLiteConnInfo(..)) where

import Control.Applicative
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Database.Schememilk.Types

newtype SQLiteConnInfo = SQLiteConnInfo { unSQLiteConnInfo :: String }

instance Backend SQLite.Connection SQLiteConnInfo where
    connect  i   = SQLite.open (unSQLiteConnInfo i)
    close        = SQLite.close

    execute_ c t   = SQLite.execute_ c (SQLite.Query $ unQuery t)

    begin    c     = SQLite.execute_ c "BEGIN TRANSACTION"
    commit   c     = SQLite.execute_ c "COMMIT TRANSACTION"
    rollback c     = SQLite.execute_ c "ROLLBACK TRANSACTION"
    withTransaction = SQLite.withTransaction

    createAdminTable c = execute_ c $ Query $ T.unlines
        [ "CREATE TABLE _scheme_milk ("
        ,   "id         INTEGER     PRIMARY KEY AUTOINCREMENT,"
        ,   "version    VARCHAR(40),"
        ,   "applied_at TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP )"
        ]
    adminTableExists c = (> 0) . asInt . SQLite.fromOnly . head <$>
        SQLite.query_ c "SELECT count(*) FROM sqlite_master WHERE name = '_scheme_milk'"

    currentVersion = defaultCurrentVersion SQLite.fromOnly SQLite.query_
    setVersion = defaultSetVersion SQLite.execute SQLite.Only

asInt :: Int -> Int
asInt = id
