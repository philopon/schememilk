module Database.Schememilk
    ( withConnection
    , upper
    , lower
    , listLog

    , up, down, current

    , Ident(..), Scheme(..)
    , Repo(..)
    , initRepo, guardAdminTable, newScheme, schemeFile
    , module Database.Schememilk.Types
    ) where

import Database.Schememilk.Types
import Database.Schememilk.Internal

up :: Backend conn ci => conn -> Repo -> IO (Maybe Ident)
up c = applyScheme ((:[]) . head) upSql c 

down :: Backend conn ci => conn -> Repo -> IO (Maybe Ident)
down c = applyScheme ((:[]) . head) dnSql c

current :: Backend conn ci => conn -> Repo -> IO (Maybe Ident)
current c = applyScheme id upSql c
