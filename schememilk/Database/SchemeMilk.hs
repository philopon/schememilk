module Database.SchemeMilk
    ( I.withConnection
    , upper
    , lower
    , listLog

    , up, down, current

    , Ident(..), I.Scheme(..)
    , module Database.SchemeMilk.Types
    ) where

import Data.Yaml
import Filesystem.Path as P
import Data.Time.Clock
import Database.SchemeMilk.Types
import qualified Database.SchemeMilk.Internal as I

upper :: Backend conn ci => conn -> P.FilePath -> IO [(Ident, UTCTime)]
upper c r = I.upper c (I.Repo r)

lower :: Backend conn ci => conn -> P.FilePath -> IO [(Ident, UTCTime)]
lower c r = I.lower c (I.Repo r)

listLog :: P.FilePath -> IO [(Ident, UTCTime, Either ParseException I.Scheme)]
listLog = I.listLog . I.Repo

up :: Backend conn ci => conn -> P.FilePath -> IO (Maybe Ident)
up c = I.applyScheme ((:[]) . head) I.upSql c . I.Repo

down :: Backend conn ci => conn -> P.FilePath -> IO (Maybe Ident)
down c = I.applyScheme ((:[]) . head) I.dnSql c . I.Repo

current :: Backend conn ci => conn -> P.FilePath -> IO (Maybe Ident)
current c = I.applyScheme id I.upSql c . I.Repo
