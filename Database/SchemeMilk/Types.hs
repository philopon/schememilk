{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Database.SchemeMilk.Types where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import Data.Maybe
import qualified Database.SQLite.Simple as SQLite

newtype Query = Query { unQuery :: T.Text }
    deriving (Show, Read, Eq)

newtype Ident = Ident { unIdent :: S.ByteString }
    deriving (Show, Eq, Ord)

class Backend a where
    data ConnectInfo a

    connect    :: ConnectInfo a -> IO a
    close      :: a -> IO ()

    execute_   :: a -> Query -> IO ()

    begin           :: a -> IO ()
    commit          :: a -> IO ()
    rollback        :: a -> IO ()
    withTransaction :: a -> IO b -> IO b

    currentVersion  :: a -> IO (Maybe Ident)
    setVersion      :: a -> Maybe Ident -> IO ()

    createAdminTable :: a -> IO ()
    createAdminTable c = execute_ c . Query $ T.unlines
        [ "CREATE TABLE _scheme_milk ("
        ,   "id         SERIAL      PRIMARY KEY,"
        ,   "version    VARCHAR(40),"
        ,   "applied_at TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP )"
        ]

instance Backend SQLite.Connection where
    newtype ConnectInfo SQLite.Connection = SQLiteConnInfo { unSQLiteConnInfo :: String } 
        deriving (Show, Read, Eq, Ord)

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
    currentVersion c = (join . listToMaybe . map (fmap (Ident . T.encodeUtf8) . SQLite.fromOnly)) `fmap`
        SQLite.query_ c "SELECT version from _scheme_milk ORDER BY id DESC LIMIT 1"
    setVersion c i = SQLite.execute c "INSERT INTO _scheme_milk (version) VALUES (?)"
        (SQLite.Only $ fmap (T.decodeUtf8 . unIdent) i)

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

{-
check c m = bracket_ (begin c) (rollback c) $ do
    a <- map reader <$> SQLite.query_ c queryStr
    execute c m
    b <- map reader <$> SQLite.query_ c queryStr
    return (a, b)
  where
    reader (t,n,tn,s) = (T.concat [t, "|", n, "|", tn], canonicalizeSql s)
    queryStr = "SELECT type, name, tbl_name sql FROM sqlite_master"
    -}
