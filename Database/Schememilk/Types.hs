{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Database.Schememilk.Types where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import Data.Maybe
import Data.String

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
