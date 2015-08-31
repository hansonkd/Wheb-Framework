
data MakeMigration = MakeMigration {isData :: Bool}


data RunMigrations = RunMigrations {fake :: Bool}

data MigrationRecord = MigrationRecord {migrationId :: MigrationId}

data Migration m = Migration (m ())

data WatchedEntity = forall a . (PersistEntity a) => WatchedEntity a

data EntityCollection = EntityCollection [WatchedEntity]


class PersistBackend m => BackupManager m where
    createBackup :: (Maybe FilePath) -> m FilePath
    restoreBackup :: FilePath -> m ()
    runMigration ::  Migration m -> m ()