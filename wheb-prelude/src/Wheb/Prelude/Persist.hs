{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs, TemplateHaskell, QuasiQuotes, StandaloneDeriving #-}

module Wheb.Prelude.Persist where

import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Reader (ReaderT(..))
import Database.Persist
import Database.Persist.Sqlite
import Data.Pool
import Wheb

class PersistApp g where
    type PersistBackend g :: *

    getPersistPool :: g -> Pool (PersistBackend g)
    runPersistReader :: (MonadBaseControl IO m, MonadIO m) => ReaderT (PersistBackend g) (WhebT g s m) a -> Pool (PersistBackend g) -> WhebT g s m a

runDb :: (MonadBaseControl IO m, MonadIO m, PersistApp g) => ReaderT (PersistBackend g) (WhebT g s m) a -> WhebT g s m a
runDb a = do
    pool <- getWithApp getPersistPool
    runPersistReader a pool

{-
data SimpleReg = SimpleReg


instance RegistrationBackend SimpleReg where
    type RegistrationUser SimpleReg = MyUser
    type ActivationToken SimpleReg = String
    type RegistrationInfo SimpleReg = String

    activate _ i = runDb $ update [IsActiveField =. True] (RegTokenField ==. i) >> return True
    register _ i = runDb $ insert $ MyUser i "addf" "d" False


data MyApp = MyApp (Maybe MyUser) (Pool Sqlite)
data MyState = MyState (Maybe MyUser) deriving (Show)

instance WhebApp MyApp where
    type DefaultRegistration MyApp = SimpleReg
    type DefaultDatabase MyApp = Sqlite
    type DefaultSession MyApp = Sqlite
    type WhebState MyApp = MyState

    getCurrentUser (MyState u) = u
    modifyAuthUser fn (MyState u) = MyState (fn u)
    getRegistrationBackend = const SimpleReg
    getPersistApp (MyApp _ db) = db

main = do

    opts <- generateOptions $ do
        pool <- createSqlitePool ":memory:" 5
        return (MyApp Nothing pool, MyState Nothing)

    runRawHandler opts $ do
        reg <- getWithApp getRegistrationBackend
        activate reg "what"
        runDb $ do
            let runAndShow sql = queryRaw False sql [] (>>= liftIO . print)
            runAndShow "select 'Groundhog embedded in arbitrary monadic context'"


-}