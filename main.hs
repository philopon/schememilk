{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}

import Control.Monad
import Control.Exception
import Prelude hiding (writeFile)
import System.IO hiding (writeFile)
import Filesystem
import Filesystem.Path.CurrentOS
import Database.SchemeMilk.Internal
import Database.SchemeMilk.Types
import Options.Applicative hiding (helper)
import qualified Database.SQLite.Simple as SQLite
import qualified Database.PostgreSQL.Simple as PSql
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as SC
import qualified Data.Yaml as Yaml
import Data.Monoid
import Data.Maybe
import System.Environment
import System.Process
import Data.Word

withConfig :: Bool -> Config -> (forall b. Backend b => b -> IO a) -> IO a
withConfig _     (SQLite a)         f = withConnection a f
withConfig askPw (PSql h p u mbw d) f = (PSqlConnInfo <$> getCi) >>= \ci -> withConnection ci f
  where
    getCi = if askPw
            then getPassword "password: " >>= \w -> return $ PSql.ConnectInfo h p u w d
            else return $ PSql.ConnectInfo h p u (maybe "" id mbw) d

withSavedConfig :: Bool -> Repo -> (forall b. Backend b => b -> IO a) -> IO a
withSavedConfig askpw repo m = readConfig repo >>= \b -> withConfig askpw b m

data Config
    = SQLite { sqliteConnInfo :: ConnectInfo SQLite.Connection }
    | PSql   { psqlHost :: String
             , psqlPort :: Word16
             , psqlUser :: String
             , psqlPass :: Maybe String
             , psqlDb   :: String
             }

getPassword :: String -> IO String
getPassword prompt = do
    putStr prompt
    hFlush stdout
    bracket
        (hGetEcho stdout >>= \e -> hSetEcho stdout False >> return e)
        (\e -> hSetEcho stdout e)
        (\_ -> getLine >>= \r -> putChar '\n' >> return r)

instance Yaml.FromJSON Config where
    parseJSON (Yaml.Object o) = do 
        backend <- o Yaml..: "backend"
        case backend :: String of
            "sqlite"     -> SQLite . SQLiteConnInfo <$> o Yaml..: "connect_info"
            "postgresql" -> pci
            _            -> fail "unknown backend."
      where
        pci = PSql
            <$> o Yaml..:  "host"
            <*> o Yaml..:  "port"
            <*> o Yaml..:  "user"
            <*> o Yaml..:? "password"
            <*> o Yaml..:  "database"
    parseJSON _ = fail "not object"

instance Yaml.ToJSON Config where
    toJSON (SQLite (SQLiteConnInfo ci)) = Yaml.object 
        [ "backend"      Yaml..= ("sqlite" :: T.Text)
        , "connect_info" Yaml..= ci
        ]
    toJSON (PSql h p u mbw d) = Yaml.object $ maybe id ((:) . ("password" Yaml..=)) mbw $
        [ "backend"  Yaml..= ("postgresql" :: T.Text)
        , "host"     Yaml..= h
        , "port"     Yaml..= p
        , "user"     Yaml..= u
        , "database" Yaml..= d
        ]

writeConfig :: Repo -> Config -> IO ()
writeConfig repo be = Yaml.encodeFile (encodeString $ repoDirectory repo </> "config") be

readConfig :: Repo -> IO Config
readConfig repo =
    Yaml.decodeFile (encodeString $ repoDirectory repo </> "config") >>=
    maybe (fail "cannot read config file.") return 

helper :: Parser (a -> a)
helper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , help "Show this help text"
  , hidden ]

config :: Maybe String -> Parser Config
config user = subparser $
    command "sqlite"     (info (helper <*> sqlite) $ progDesc "sqlite backend") <>
    command "postgresql" (info (helper <*> psql)   $ progDesc "postgresql backend")
  where
    sqlite = SQLite . SQLiteConnInfo <$> argument str (metavar "DB" <> help "sqlite database file.")
    showDefStr = showDefaultWith id
    psql   = PSql
        <$> strOption (short 'h' <> long "host" <> value "localhost" <> metavar "HOST" <> help "postgresql host" <> showDefStr)
        <*> option    (short 'p' <> long "port" <> value 5432        <> metavar "PORT" <> help "postgresql port" <> showDefault)
        <*> strOption (short 'u' <> long "user" <> maybe mempty value user <> metavar "USER" <> help "postgresql user" <> showDefStr) 
        <*> optional (strOption $ short 'w' <> long "password" <> metavar "PWD" <> help "postgresql password")
        <*> strOption (short 'd' <> long "database" <> maybe mempty value user <> metavar "DB" <> help "postgresql database" <> showDefStr)

data Action
    = InitRepo   { optConfig :: Config, askPassword :: Bool}
    | ShowStatus { askPassword :: Bool }
    | ShowLog
    | NewScheme{ askPassword :: Bool }
    | UpScheme{ askPassword :: Bool }
    | DownScheme{ askPassword :: Bool }
    | CurrentScheme{ askPassword :: Bool }

options :: Maybe String -> Parser Action
options user = subparser $ mconcat
    [ (command "status"  $ info (helper <*> (ShowStatus <$> pw)) $ progDesc "show status")
    , (command "init"    $ info (helper <*> (InitRepo <$> config user <*> pw)) $ progDesc "init repo")
    , (command "log"     $ info (helper <*> pure ShowLog)    $ progDesc "show log")
    , (command "new"     $ info (helper <*> (NewScheme <$> pw))  $ progDesc "create new scheme")
    , (command "up"      $ info (helper <*> (UpScheme <$> pw))   $ progDesc "up one scheme version")
    , (command "down"    $ info (helper <*> (DownScheme <$> pw)) $ progDesc "down one scheme version")
    , (command "current" $ info (helper <*> (CurrentScheme <$> pw)) $ progDesc "up to current scheme version")
    ]
  where
    pw = flag False True (short 'W' <> long "prompt" <> help "password prompt.")

getEditor :: IO String
getEditor = maybe "vim" id <$> do
    (<|>) <$> lookupEnv "SCHEMEMILK_EDITOR" <*> lookupEnv "EDITOR"

main :: IO ()
main = do
    user <- lookupEnv "USER"
    execParser (info (helper <*> options user) fullDesc) >>= doAction repo
  where
    repo = Repo ".schememilk"

doAction :: Repo -> Action -> IO ()
doAction repo InitRepo{..} = withConfig askPassword optConfig $ \conn -> do
    initRepo conn repo
    writeConfig repo optConfig
    writeFile (repoDirectory repo </> "template.yml") "description: \nup: \ndown: \n"

doAction repo ShowStatus{..} = withSavedConfig askPassword repo $ \conn ->
    (showf <$> currentVersion conn <*> upper conn repo) >>= SC.putStrLn
  where
    showf v       [] = maybe "no schema" (\v' -> SC.concat ["db version: ", unIdent v', "(latest)"]) v
    showf Nothing l  = 
        SC.concat ["not migrated(", SC.pack . show $ length l, " schema not applied exists.)."]
    showf (Just v) l =
        SC.concat ["db version: ", unIdent v, "(", SC.pack . show $ length l, "schema not applied exists.)"]

doAction repo ShowLog{} =
    let showLog (i, _, Left e)  = hPutStrLn stderr . unwords $
            ["Warning: error occored on", SC.unpack $ unIdent i, "(", show e, ")"]
        showLog (i, d, Right s) = do
            SC.putStr (SC.concat [SC.pack $ show d, "\t", unIdent i, "\t"]) 
            maybe (putChar '\n') T.putStrLn (description s)
    in listLog repo >>= mapM_ showLog . reverse

doAction repo NewScheme{} = do
    (idnt, cmt, rbk) <- newScheme repo (repoDirectory repo </> "template.yml")
    e     <- getEditor
    origt <- getModified (schemeFile repo idnt)
    callCommand $ e ++ ' ': encodeString (schemeFile repo idnt)
    modt  <- getModified (schemeFile repo idnt)
    if origt == modt
        then rbk      >> putStrLn "not modified."
        else cmt modt >> putStrLn "new schema created."

doAction repo UpScheme{..}  = withSavedConfig askPassword repo $ \conn -> upper conn repo >>= \case
    []      -> putStrLn "newest."
    (i,_):_ -> Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
        Left  e -> print e
        Right s -> do 
            SC.putStrLn (SC.unwords ["up to", unIdent i]) 
            withTransaction conn $ mapM_ (execute_ conn) (upSql s) >> setVersion conn (Just i)

doAction repo DownScheme{..} = withSavedConfig askPassword repo $ \conn -> lower conn repo >>= \case
    []      -> putStrLn "oldest."
    (i,_):o -> SC.putStrLn (SC.unwords ["down from", unIdent i]) >> 
        Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
            Left  e -> print e
            Right s -> do 
                withTransaction conn $ mapM_ (execute_ conn) (dnSql s) 
                setVersion conn (fst <$> listToMaybe o)

doAction repo CurrentScheme{..} = withSavedConfig askPassword repo $ \conn -> upper conn repo >>= \case
    []  -> putStrLn "newest."
    ss  -> withTransaction conn $ forM_ ss $ \(i,_) ->
        Yaml.decodeFileEither (encodeString $ schemeFile repo i) >>= \case
            Left  e -> print e
            Right s -> do
                SC.putStrLn (SC.unwords ["up to", unIdent i]) 
                mapM_ (execute_ conn) (upSql s) >> setVersion conn (Just i)
