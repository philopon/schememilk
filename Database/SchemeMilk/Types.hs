{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Database.SchemeMilk.Types where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import Data.Maybe
import Data.String
import qualified Database.SQLite.Simple as SQLite
import qualified Database.PostgreSQL.Simple as PSql
import qualified Database.PostgreSQL.Simple.Types as PSql

newtype Query = Query { unQuery :: T.Text }
    deriving (Show, Read, Eq)

newtype Ident = Ident { unIdent :: S.ByteString }
    deriving (Show, Eq, Ord)

class Backend conn ci | conn -> ci, ci -> conn where
    connect    :: ci   -> IO conn
    close      :: conn -> IO ()

    execute_   :: conn -> Query -> IO ()

    begin           :: conn -> IO ()
    commit          :: conn -> IO ()
    rollback        :: conn -> IO ()
    withTransaction :: conn -> IO b -> IO b

    currentVersion  :: conn -> IO (Maybe Ident)
    setVersion      :: conn -> Maybe Ident -> IO ()

    createAdminTable :: conn -> IO ()
    createAdminTable c = execute_ c . Query $ T.unlines
        [ "CREATE TABLE _scheme_milk ("
        ,   "id         SERIAL      PRIMARY KEY,"
        ,   "version    VARCHAR(40),"
        ,   "applied_at TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP )"
        ]
    adminTableExists :: conn -> IO Bool

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

defaultCurrentVersion :: IsString q
                      => (v -> Maybe T.Text) -> (c -> q -> IO [v]) -> c -> IO (Maybe Ident)
defaultCurrentVersion fromOnly query_ c = 
    (join . listToMaybe . map (fmap (Ident . T.encodeUtf8) . fromOnly)) `fmap`
    query_ c "SELECT version FROM _scheme_milk ORDER BY id DESC LIMIT 1"

defaultSetVersion :: IsString q
                  => (c -> q -> v -> IO a) -> (Maybe T.Text -> v) -> c -> Maybe Ident -> IO ()
defaultSetVersion execute only c i = void $
    execute c "INSERT INTO _scheme_milk (version) VALUES (?)"
    (only $ fmap (T.decodeUtf8 . unIdent) i)

{-
canonicalizeSql :: T.Text -> T.Text
canonicalizeSql = T.concat . map (reduce . T.unpack) . T.groupBy gf 
  where
    gf a b 
        | notElem a " \n\r()," = False
        | b `elem` " \n\r(),"  = True
        | otherwise            = False
    reduce s 
        | '(' `elem` s  = "("
        | ')' `elem` s  = ")"
        | ',' `elem` s  = ", "
        | length s == 1 = T.singleton $ head s
        | otherwise     = " "

check c m = bracket_ (begin c) (rollback c) $ do
    a <- map reader <$> SQLite.query_ c queryStr
    execute c m
    b <- map reader <$> SQLite.query_ c queryStr
    return (a, b)
  where
    reader (t,n,tn,s) = (T.concat [t, "|", n, "|", tn], canonicalizeSql s)
    queryStr = "SELECT type, name, tbl_name sql FROM sqlite_master"
    -}
