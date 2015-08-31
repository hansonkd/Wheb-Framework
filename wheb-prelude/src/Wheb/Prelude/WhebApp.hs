{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs, TemplateHaskell, QuasiQuotes, StandaloneDeriving #-}

module Wheb.Prelude.WhebApp where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Pool
import Data.Default
import Wheb
import Wheb.Plugins.Session
import Wheb.Prelude.Persist


import           Control.Monad.Reader (ReaderT(ReaderT), MonadReader(..))


type PreludeApp g = WhebT g (WhebState g) IO



class (PersistEntity (RegistrationUser c)) => RegistrationBackend c where
    type RegistrationUser c :: *
    type ActivationToken c :: *
    type RegistrationInfo c :: *

    -- | What does this mean?
    activate :: (WhebApp g, PersistEntityBackend (RegistrationUser c) ~ (PersistBackend g)) => c -> ActivationToken c -> PreludeApp g Bool
    register :: (WhebApp g, PersistEntityBackend (RegistrationUser c) ~ (PersistBackend g)) => c -> RegistrationInfo c -> PreludeApp g (Key (RegistrationUser c))


instance (WhebApp g) => SessionApp g where
    type SessionAppBackend g = DefaultSession g
    getSessionContainer = getSessionBackend


class (PersistApp g, SessionApp g) => WhebApp g where

    type DefaultRegistration g :: *

    type WhebUser g :: *
    type WhebUser g = RegistrationUser (DefaultRegistration g)

    type DefaultSession g :: *
    type WhebState g :: *

    getCurrentUser :: WhebState g -> Maybe (WhebUser g)
    modifyAuthUser :: (Maybe (WhebUser g) -> Maybe (WhebUser g)) -> WhebState g -> WhebState g

    getRegistrationBackend :: (RegistrationBackend (DefaultRegistration g)) => g -> DefaultRegistration g
    getSessionBackend :: g -> DefaultSession g


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
MyUser
    name String
    regToken String
    isActive Bool
    deriving Show
|]

data UniversalBackend = forall

data SimpleReg = SimpleReg

instance RegistrationBackend SimpleReg where
    type RegistrationUser SimpleReg = MyUser
    type ActivationToken SimpleReg = String
    type RegistrationInfo SimpleReg = String

    activate _ i = runDb $ updateWhere [MyUserRegToken ==. i] [MyUserIsActive =. True] >> return undefined
    register _ i = runDb $ insert $ MyUser "addf" "d" False


data MyApp = MyApp (Maybe MyUser) (Pool SqlBackend)
data MyState = MyState (Maybe MyUser) deriving (Show)

instance PersistApp MyApp where
    type PersistBackend MyApp = SqlBackend
    getPersistPool (MyApp _ db) = db
    runPersistReader = runSqlPool

instance WhebApp MyApp where
    type DefaultRegistration MyApp = SimpleReg
    type DefaultSession MyApp = SqliteConf
    type WhebState MyApp = MyState

    getCurrentUser (MyState u) = u
    modifyAuthUser fn (MyState u) = MyState (fn u)
    getRegistrationBackend = const SimpleReg

main = do

    opts <- generateOptions $ do
        let sqlConf = SqliteConf ":memory:" 5
        pool <- liftIO $ createPoolConfig sqlConf
        return (MyApp Nothing (pool), MyState Nothing)

    runRawHandler opts $ do
        reg <- getWithApp getRegistrationBackend
        activate reg "what"
