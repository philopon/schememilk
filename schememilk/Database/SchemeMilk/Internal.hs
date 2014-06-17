{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Database.SchemeMilk.Internal where

import Prelude hiding (FilePath, readFile, appendFile)

import Control.Applicative
import Data.Yaml
import Database.SchemeMilk.Types
import Filesystem
import Filesystem.Path.CurrentOS
import System.Random.MWC

import Control.Exception
import Control.Monad
import Control.Monad.Primitive

import Data.Word
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

schemeDirectory :: Repo -> FilePath
schemeDirectory (Repo p) = p </> "scheme"

schemeFile :: Repo -> Ident -> FilePath
schemeFile repo (Ident idnt) = schemeDirectory repo </> decodeString (SC.unpack idnt)

initRepo :: Backend c ci => c -> Repo -> IO ()
initRepo conn d = do
    createDirectory False $ repoDirectory d
    createDirectory False $ schemeDirectory d
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

newScheme :: Repo
          -> FilePath -- ^ template
          -> IO (Ident
                , UTCTime -> IO () -- commit
                , IO () -- rollback
                )
newScheme repo temp = do
    hist  <- readHistoryFile repo
    newId <- withSystemRandom . asGenIO $ newIdent (Set.fromList $ map fst hist)
    copyFile temp $ schemeFile repo newId
    let commitf time = do
            appendFile (historyFile repo) $ unIdent newId `SC.append` SC.pack (' ' : show time ++ "\n")
        rollbackf = removeFile $ schemeFile repo newId
    return (newId, commitf, rollbackf)


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

data Scheme = Scheme
    { description :: Maybe T.Text
    , upSql :: [Query]
    , dnSql :: [Query]
    } deriving Show

instance FromJSON Scheme where
    parseJSON (Object o) = Scheme <$> (o .:? "description") <*> (o .: "up" >>= f) <*> (o .: "down" >>= f)
      where 
        f a = fmap (map Query)     (parseJSON a) <|> 
              fmap ((:[]) . Query) (parseJSON a)
    parseJSON _ = mzero

listLog :: Repo -> IO [(Ident, UTCTime, Either ParseException Scheme)]
listLog repo = readHistoryFile repo >>=
    mapM (\(i,t) -> (i,t,) <$> (decodeFileEither . encodeString $ schemeFile repo i))

left :: (l -> l') -> Either l r -> Either l' r
left f (Left a)  = Left $ f a
left _ (Right a) = Right a

guardAdminTable :: Backend conn ci => conn -> IO ()
guardAdminTable c = adminTableExists c >>= \b -> if b then return () else createAdminTable c

applyScheme :: Backend conn ci => (forall a. [a] -> [a])
            -> (Scheme -> [Query]) -> conn -> Repo -> IO (Maybe Ident)
applyScheme f g conn repo = withTransaction conn (guardAdminTable conn >> upper conn repo) >>= \case
    [] -> return Nothing
    ss -> withTransaction conn $ foldM (\_ (i, _) ->
        decodeFileEither (encodeString $ schemeFile repo i) >>= \case
            Left  e -> throwIO e
            Right s -> do
                mapM_ (execute_ conn) (g s) 
                setVersion conn (Just i) 
                return $ Just i
            ) Nothing (f ss)
