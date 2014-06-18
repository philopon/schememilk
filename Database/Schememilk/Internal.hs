{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Database.Schememilk.Internal where

import Prelude hiding (FilePath, readFile, appendFile)

import Control.Applicative
import Data.Yaml
import Database.Schememilk.Types
import Filesystem
import Filesystem.Path.CurrentOS
import System.Random.MWC

import Control.Exception
import Control.Monad
import Control.Monad.Primitive

import Data.Word
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.Text as T
import Data.String(IsString)
import Data.Time

withConnection :: Backend c ci => ci -> (c -> IO b) -> IO b
withConnection ci = bracket (connect ci) close

touchFile :: FilePath -> IO ()
touchFile f = withFile f AppendMode (\_ -> return ())

newtype Repo = Repo { repoDirectory :: FilePath }
    deriving (Show, Eq, Ord)

deriving instance IsString Repo

historyFile :: Repo -> FilePath
historyFile (Repo p) = p </> "history.txt"

schemaDirectory :: Repo -> FilePath
schemaDirectory (Repo p) = p </> "schema"

schemaFile :: Repo -> Ident -> FilePath
schemaFile repo (Ident idnt) = schemaDirectory repo </> decodeString (SC.unpack idnt)

initRepo :: Backend c ci => c -> Repo -> IO ()
initRepo conn d = do
    createDirectory False $ repoDirectory d
    createDirectory False $ schemaDirectory d
    touchFile $ historyFile d
    createAdminTable conn

readHistoryFile :: Repo -> IO [(Ident, UTCTime)]
readHistoryFile f = map readf . SC.lines <$> readFile (historyFile f)
  where
    readf s = let (i,o) = SC.break (== ' ') s
                  d     = read . tail $ SC.unpack o
              in (Ident i, d)

idChar :: S.ByteString
idChar = "23456789ABCDEFGHJKLMNOPRSTUVWXYZabcdefghijkmnapqrstuvwxyz"

uniformIdChar :: PrimMonad m => Gen (PrimState m) -> m Word8
uniformIdChar g = (idChar `S.index`) `liftM` uniformR (0, S.length idChar - 1) g

newIdent :: PrimMonad m => Set.Set Ident -> Gen (PrimState m) -> m Ident
newIdent l g = do
    idnt <- (Ident . S.pack) `liftM` replicateM 8 (uniformIdChar g)
    if Set.member idnt l then newIdent l g else return idnt

data NewSchema = NewSchema
    { newSchemaIdent    :: Ident
    , commitNewSchema   :: UTCTime -> IO ()
    , rollbackNewSchema :: IO ()
    }

newSchema :: Repo
          -> FilePath -- ^ template
          -> IO NewSchema
newSchema repo temp = do
    hist  <- readHistoryFile repo
    newId <- withSystemRandom . asGenIO $ newIdent (Set.fromList $ map fst hist)
    copyFile temp $ schemaFile repo newId
    let commitf time = do
            appendFile (historyFile repo) $ unIdent newId `SC.append` SC.pack (' ' : show time ++ "\n")
        rollbackf = removeFile $ schemaFile repo newId
    return $ NewSchema newId commitf rollbackf


upper :: Backend c ci => c -> Repo -> IO [(Ident, UTCTime)]
upper conn repo = (,) <$> currentVersion conn <*> readHistoryFile repo >>= \case
    (Nothing, h) -> return h
    (Just s,  h) -> case dropWhile ((s /=) . fst) h of
        []  -> return h
        [_] -> return []
        a   -> return $ tail a

lower :: Backend c ci => c -> Repo -> IO [(Ident, UTCTime)]
lower conn repo = (,) <$> currentVersion conn <*> readHistoryFile repo >>= \case
    (Nothing, _) -> return []
    (Just s,  h) -> case dropWhile ((s /=) . fst) $ reverse h of
        [] -> return h
        a  -> return a

data Schema = Schema
    { description :: Maybe T.Text
    , upSql :: [Query]
    , dnSql :: [Query]
    } deriving Show

instance FromJSON Schema where
    parseJSON (Object o) = Schema <$> (o .:? "description") <*> (o .: "up" >>= f) <*> (o .: "down" >>= f)
      where 
        f a = fmap (map Query)     (parseJSON a) <|> 
              fmap ((:[]) . Query) (parseJSON a)
    parseJSON _ = mzero

listLog :: Repo -> IO [(Ident, UTCTime, Either ParseException Schema)]
listLog repo = readHistoryFile repo >>=
    mapM (\(i,t) -> (i,t,) <$> (decodeFileEither . encodeString $ schemaFile repo i))

left :: (l -> l') -> Either l r -> Either l' r
left f (Left a)  = Left $ f a
left _ (Right a) = Right a

guardAdminTable :: Backend conn ci => conn -> IO ()
guardAdminTable c = adminTableExists c >>= \b -> if b then return () else createAdminTable c

applySchema :: Backend conn ci
            => (forall a. [a] -> [a])
            -> conn -> Repo -> IO (Maybe Ident)
applySchema f conn repo = withTransaction conn (guardAdminTable conn >> upper conn repo) >>= \case
    [] -> return Nothing
    ss -> withTransaction conn $ foldM (\_ (i, _) ->
        decodeFileEither (encodeString $ schemaFile repo i) >>= \case
            Left  e -> throwIO e
            Right s -> do
                mapM_ (execute_ conn) (upSql s) 
                setVersion conn (Just i) 
                return $ Just i
            ) Nothing (f ss)

up :: Backend conn ci => conn -> Repo -> IO (Maybe Ident)
up = applySchema ((:[]) . head)

current :: Backend conn ci => conn -> Repo -> IO (Maybe Ident)
current = applySchema id

down :: Backend conn ci => conn -> Repo -> IO (Maybe Ident)
down conn repo = lower conn repo >>= \case
    []      -> return Nothing
    (i,_):o -> decodeFileEither (encodeString $ schemaFile repo i) >>= \case
            Left  e -> throwIO e
            Right s -> do 
                withTransaction conn $ mapM_ (execute_ conn) (dnSql s) 
                setVersion conn (fst <$> listToMaybe o)
                return $ Just i
