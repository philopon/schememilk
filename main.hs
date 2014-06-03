{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}

import Control.Monad
import Prelude hiding (writeFile)
import System.IO hiding (writeFile)
import Filesystem
import Filesystem.Path.CurrentOS
import Database.SchemeMilk.Internal
import Database.SchemeMilk.Types
import Options.Applicative
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as SC
import qualified Data.Yaml as Yaml
import Data.Monoid
import Data.Maybe

withConfig :: Config -> (forall b. Backend b => b -> IO a) -> IO a
withConfig (SQLite a) f = withConnection a f

withSavedConfig :: Repo -> (forall b. Backend b => b -> IO a) -> IO a
withSavedConfig repo m = readConfig repo >>= \b -> withConfig b m

data Config
    = SQLite { sqliteConnInfo :: ConnectInfo SQLite.Connection }
    deriving (Show, Read)

writeConfig :: Repo -> Config -> IO ()
writeConfig repo be = writeTextFile (repoDirectory repo </> "config") (T.pack $ show be)

readConfig :: Repo -> IO Config
readConfig repo = read . T.unpack <$> readTextFile (repoDirectory repo </> "config")

config :: Parser Config
config = (nullOption (short 'b' <> reader (\s -> if s == "sqlite" then return SQLite else fail ""))) <*> (SQLiteConnInfo <$> strOption (short 's'))

data Options 
    = InitRepo { optConfig :: Config }
    | ShowStatus
    | ShowLog
    | NewScheme
    | UpScheme
    | DownScheme
    | CurrentScheme

options :: Parser Options
options = subparser $ mconcat
    [ (command "status"  $ info (helper <*> pure ShowStatus) $ progDesc "show status")
    , (command "init"    $ info (helper <*> (InitRepo <$> config)) $ progDesc "init repo")
    , (command "log"     $ info (helper <*> pure ShowLog)    $ progDesc "show log")
    , (command "new"     $ info (helper <*> pure NewScheme)  $ progDesc "create new scheme")
    , (command "up"      $ info (helper <*> pure UpScheme)   $ progDesc "up one scheme version")
    , (command "down"    $ info (helper <*> pure DownScheme) $ progDesc "down one scheme version")
    , (command "current" $ info (helper <*> pure CurrentScheme) $ progDesc "up to current scheme version")
    ]

main :: IO ()
main = execParser (info (helper <*> options) fullDesc) >>= \case
    InitRepo{..} -> withConfig optConfig $ \conn -> do
        initRepo conn repo
        writeConfig repo optConfig

    ShowStatus   -> withSavedConfig repo $ \conn -> do
        print =<< currentVersion conn
        print =<< upper conn repo
        print =<< lower conn repo

    ShowLog      ->
        let showLog (i,  Left e) = hPutStrLn stderr . unwords $
                ["Warning: error occored on", SC.unpack $ unIdent i, "(", show e, ")"]
            showLog (i, Right s) = SC.putStr (unIdent i `SC.snoc` '\t') >> maybe (putChar '\n') T.putStrLn (description s)
        in listLog repo >>= mapM_ showLog . reverse

    NewScheme    -> newScheme repo "template.yml" >>= print

    UpScheme     -> withSavedConfig repo $ \conn -> upper conn repo >>= \case
        []  -> putStrLn "newest."
        i:_ -> Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
            Left  e -> print e
            Right s -> do 
                SC.putStrLn (SC.unwords ["up to", unIdent i]) 
                withTransaction conn $ mapM_ (execute_ conn) (upSql s) >> setVersion conn (Just i)

    DownScheme   -> withSavedConfig repo $ \conn -> lower conn repo >>= \case
        []  -> putStrLn "oldest."
        i:o -> SC.putStrLn (SC.unwords ["down from", unIdent i]) >> Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
            Left  e -> print e
            Right s -> withTransaction conn $ mapM_ (execute_ conn) (dnSql s) >> setVersion conn (listToMaybe o)

    CurrentScheme -> withSavedConfig repo $ \conn -> upper conn repo >>= \case
        []  -> putStrLn "newest."
        ss  -> withTransaction conn $ forM_ ss $ \i -> Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
            Left  e -> print e
            Right s -> do
                SC.putStrLn (SC.unwords ["up to", unIdent i]) 
                mapM_ (execute_ conn) (upSql s) >> setVersion conn (Just i)
 where 
    repo = Repo ".schememilk"

