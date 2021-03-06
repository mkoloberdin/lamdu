module Lamdu.Data.Db
    ( withDB
    , withDBOpts, ImplicitFreshDb(..)
    ) where

import           Control.Exception (onException)
import qualified Lamdu.Data.Db.Init as DbInit
import           Lamdu.Data.Db.Layout (DbM(..), ViewM)
import           Lamdu.Data.Db.Migration (migration)
import           Lamdu.Data.Export.JSON (fileImportAll)
import qualified Lamdu.Paths as Paths
import qualified Revision.Deltum.Db as Db
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Lamdu.Prelude

type T = Transaction

data ImplicitFreshDb = ImplicitFreshDb | NoImplicitFreshDb | FailIfFresh String
    deriving (Eq, Show)

importFreshDb :: ImplicitFreshDb -> IO (T ViewM ())
importFreshDb NoImplicitFreshDb = pure (pure ())
importFreshDb (FailIfFresh msg) = fail msg
importFreshDb ImplicitFreshDb =
    Paths.getDataFileName "freshdb.json" >>= fileImportAll

withDB :: FilePath -> (Transaction.Store DbM -> IO a) -> IO a
withDB path = withDBOpts path ImplicitFreshDb

withDBOpts :: FilePath -> ImplicitFreshDb -> (Transaction.Store DbM -> IO a) -> IO a
withDBOpts lamduDir implicitFreshDb body =
    do
        Directory.createDirectoryIfMissing False lamduDir
        alreadyExist <- Directory.doesDirectoryExist dbPath
        let options =
                Db.defaultOptions
                { Db.createIfMissing = not alreadyExist
                , Db.errorIfExists = not alreadyExist
                }
        Db.withDB dbPath options $
            \ioDb ->
            do
                let db = Transaction.onStoreM DbM ioDb
                if alreadyExist
                    then migration db
                    else
                    (importFreshDb implicitFreshDb >>= DbInit.initDb db)
                    `onException` Directory.removeDirectoryRecursive dbPath
                body db
    where
        dbPath = lamduDir </> "codeedit.db"
