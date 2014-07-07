{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Schememilk.Internal where

import Prelude hiding (FilePath, readFile, appendFile)

import Control.Applicative
import Control.Monad
import System.Random.MWC

import Filesystem
import Filesystem.Path.CurrentOS

import Data.Word
import Data.Time
import Data.String
import qualified Data.Set as Set
import qualified Data.Text       as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

import Database.Sql.Simple

newtype Repo = Repo { repoDirectory :: FilePath }
    deriving (Show, Eq, Ord, IsString)

newtype Ident = Ident { unIdent :: S.ByteString }
    deriving (Show, Eq, Ord)

touchFile :: FilePath -> IO ()
touchFile f = withFile f AppendMode (const $ return ())

historyFile :: Repo -> FilePath
historyFile (Repo p) = p </> "history.txt"

schemaDirectory :: Repo -> FilePath
schemaDirectory (Repo p) = p </> "schema"

schemaFile :: Repo -> Ident -> FilePath
schemaFile repo (Ident i) = schemaDirectory repo </> decodeString (SC.unpack i) <.> "yml"

initRepo :: Repo -> IO ()
initRepo d = do
    createDirectory False $ repoDirectory   d
    createDirectory False $ schemaDirectory d
    touchFile $ historyFile d

readHistoryFile :: Repo -> IO [(Ident, UTCTime)]
readHistoryFile f = map readf . SC.lines <$> readFile (historyFile f)
  where
    readf s = let (i, o) = SC.break (== ' ') s
                  d      = read . tail $ SC.unpack o
              in (Ident i, d)

idChar :: S.ByteString
idChar = "23456789ABCDEFGHJKLMNOPRSTUVWXYZabcdefghijkmnapqrstuvwxyz"

uniformIdChar :: GenIO -> IO Word8
uniformIdChar g = (idChar `S.index`) `liftM` uniformR (0, S.length idChar - 1) g

newIdent :: Set.Set Ident -> GenIO -> IO Ident
newIdent l g = do
    ident <- (Ident . S.pack) `liftM` replicateM 8 (uniformIdChar g)
    if Set.member ident l then newIdent l g else return ident

data NewSchema = NewSchema
    { newSchemaIdent    :: Ident
    , commitNewSchema   :: UTCTime -> IO ()
    , rollbackNewSchema :: IO ()
    }

newSchema :: Repo -> FilePath -> IO NewSchema
newSchema repo temp = do
    hist  <- readHistoryFile repo
    newId <- withSystemRandom $ newIdent (Set.fromList $ map fst hist)
    copyFile temp $ schemaFile repo newId
    let commitf time =
            appendFile (historyFile repo) $ unIdent newId `SC.append` SC.pack (' ' : show time ++ "\n")
        rollbackf    = removeFile $ schemaFile repo newId
    return $ NewSchema newId commitf rollbackf

data Schema = Schema
    { description :: Maybe T.Text
    , upSql       :: [Query]
    , downSql     :: [Query]
    } deriving Show

