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
import System.Environment
import System.Process

withConfig :: Config -> (forall b. Backend b => b -> IO a) -> IO a
withConfig (SQLite a) f = withConnection a f

withSavedConfig :: Repo -> (forall b. Backend b => b -> IO a) -> IO a
withSavedConfig repo m = readConfig repo >>= \b -> withConfig b m

data Config
    = SQLite { sqliteConnInfo :: ConnectInfo SQLite.Connection }
    deriving (Show, Read)

instance Yaml.FromJSON Config where
    parseJSON (Yaml.Object o) = do 
        backend <- o Yaml..: "backend"
        ci <- case backend :: String of
            "sqlite" -> SQLiteConnInfo <$> o Yaml..: "connect_info"
            _        -> fail "unknown backend."
        return $ SQLite ci
    parseJSON _ = fail "not object"

instance Yaml.ToJSON Config where
    toJSON (SQLite (SQLiteConnInfo ci)) = Yaml.object [ "backend"      Yaml..= ("sqlite" :: T.Text)
                                                      , "connect_info" Yaml..= ci
                                                      ]

writeConfig :: Repo -> Config -> IO ()
writeConfig repo be = Yaml.encodeFile (encodeString $ repoDirectory repo </> "config") be

readConfig :: Repo -> IO Config
readConfig repo =
    Yaml.decodeFile (encodeString $ repoDirectory repo </> "config") >>=
    maybe (fail "cannot read config file.") return 

config :: Parser Config
config = (nullOption (short 'b' <> reader (\s -> if s == "sqlite" then return SQLite else fail ""))) <*> (SQLiteConnInfo <$> strOption (short 's'))

data Action
    = InitRepo { optConfig :: Config }
    | ShowStatus
    | ShowLog
    | NewScheme
    | UpScheme
    | DownScheme
    | CurrentScheme

options :: Parser Action
options = subparser $ mconcat
    [ (command "status"  $ info (helper <*> pure ShowStatus) $ progDesc "show status")
    , (command "init"    $ info (helper <*> (InitRepo <$> config)) $ progDesc "init repo")
    , (command "log"     $ info (helper <*> pure ShowLog)    $ progDesc "show log")
    , (command "new"     $ info (helper <*> pure NewScheme)  $ progDesc "create new scheme")
    , (command "up"      $ info (helper <*> pure UpScheme)   $ progDesc "up one scheme version")
    , (command "down"    $ info (helper <*> pure DownScheme) $ progDesc "down one scheme version")
    , (command "current" $ info (helper <*> pure CurrentScheme) $ progDesc "up to current scheme version")
    ]

getEditor :: IO String
getEditor = maybe "vim" id <$> do
    (<|>) <$> lookupEnv "SCHEMEMILK_EDITOR" <*> lookupEnv "EDITOR"

main :: IO ()
main = execParser (info (helper <*> options) fullDesc) >>= doAction repo
  where
    repo = Repo ".schememilk"

doAction :: Repo -> Action -> IO ()
doAction repo InitRepo{..} = withConfig optConfig $ \conn -> do
    initRepo conn repo
    writeConfig repo optConfig
    writeFile (repoDirectory repo </> "template.yml") "description: \nup: \ndown: \n"

doAction repo ShowStatus = withSavedConfig repo $ \conn ->
    (showf <$> currentVersion conn <*> upper conn repo) >>= SC.putStrLn
  where
    showf v       [] = maybe "no schema" (\v' -> SC.concat ["db version: ", unIdent v', "(latest)"]) v
    showf Nothing l  = 
        SC.concat ["not migrated(", SC.pack . show $ length l, " schema not applied exists.)."]
    showf (Just v) l =
        SC.concat ["db version: ", unIdent v, "(", SC.pack . show $ length l, "schema not applied exists.)"]

doAction repo ShowLog =
    let showLog (i, _, Left e)  = hPutStrLn stderr . unwords $
            ["Warning: error occored on", SC.unpack $ unIdent i, "(", show e, ")"]
        showLog (i, d, Right s) = do
            SC.putStr (SC.concat [SC.pack $ show d, "\t", unIdent i, "\t"]) 
            maybe (putChar '\n') T.putStrLn (description s)
    in listLog repo >>= mapM_ showLog . reverse

doAction repo NewScheme = do
    (idnt, cmt, rbk) <- newScheme repo (repoDirectory repo </> "template.yml")
    e     <- getEditor
    origt <- getModified (schemeFile repo idnt)
    callCommand $ e ++ ' ': encodeString (schemeFile repo idnt)
    modt  <- getModified (schemeFile repo idnt)
    if origt == modt
        then rbk      >> putStrLn "not modified."
        else cmt modt >> putStrLn "new schema created."

doAction repo UpScheme  = withSavedConfig repo $ \conn -> upper conn repo >>= \case
    []      -> putStrLn "newest."
    (i,_):_ -> Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
        Left  e -> print e
        Right s -> do 
            SC.putStrLn (SC.unwords ["up to", unIdent i]) 
            withTransaction conn $ mapM_ (execute_ conn) (upSql s) >> setVersion conn (Just i)

doAction repo DownScheme = withSavedConfig repo $ \conn -> lower conn repo >>= \case
    []      -> putStrLn "oldest."
    (i,_):o -> SC.putStrLn (SC.unwords ["down from", unIdent i]) >> 
        Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
            Left  e -> print e
            Right s -> do 
                withTransaction conn $ mapM_ (execute_ conn) (dnSql s) 
                setVersion conn (fst <$> listToMaybe o)

doAction repo CurrentScheme = withSavedConfig repo $ \conn -> upper conn repo >>= \case
    []  -> putStrLn "newest."
    ss  -> withTransaction conn $ forM_ ss $ \(i,_) ->
        Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
            Left  e -> print e
            Right s -> do
                SC.putStrLn (SC.unwords ["up to", unIdent i]) 
                mapM_ (execute_ conn) (upSql s) >> setVersion conn (Just i)

