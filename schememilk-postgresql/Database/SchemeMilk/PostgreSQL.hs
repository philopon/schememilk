{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.SchemeMilk.PostgreSQL 
    (
    PSql.ConnectInfo(..)
    ) where

import Control.Monad
import Control.Applicative
import qualified Data.Text.Encoding as T
import Database.SchemeMilk.Types
import qualified Database.PostgreSQL.Simple as PSql
import qualified Database.PostgreSQL.Simple.Types as PSql

instance Backend PSql.Connection PSql.ConnectInfo where
    connect i = PSql.connect i
    close     = PSql.close

    execute_ c t = void $ PSql.execute_ c (PSql.Query . T.encodeUtf8 $ unQuery t)

    begin    = PSql.begin
    commit   = PSql.commit
    rollback = PSql.rollback
    withTransaction = PSql.withTransaction

    adminTableExists c = (> 0) . asInt . PSql.fromOnly . head <$> 
        PSql.query_ c "SELECT count(*) FROM pg_class WHERE relname = '_scheme_milk'"

    currentVersion = defaultCurrentVersion PSql.fromOnly PSql.query_
    setVersion = defaultSetVersion PSql.execute PSql.Only

asInt :: Int -> Int
asInt = id
