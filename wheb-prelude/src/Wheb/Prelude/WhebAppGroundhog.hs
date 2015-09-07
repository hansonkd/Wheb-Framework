{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs, TemplateHaskell, QuasiQuotes, StandaloneDeriving #-}

module Main where

import Database.Groundhog
import Database.Groundhog.Generic
import Database.Groundhog.Core
import Database.Groundhog.Sqlite
import Database.Groundhog.TH
import Data.Pool
import Data.Default
import Wheb
import Wheb.Plugins.Session

import           Control.Monad.Reader (ReaderT(ReaderT), MonadReader(..))


type PreludeApp g = WhebT g (WhebState g) IO





class RegistrationBackend c where
    type RegistrationUser c :: *
    type ActivationToken c :: *
    type RegistrationInfo c :: *

    activate :: (WhebApp g) => c -> ActivationToken c -> PreludeApp g Bool
    register :: (WhebApp g, PersistEntity (RegistrationUser c)) => c -> RegistrationInfo c -> PreludeApp g (AutoKey (RegistrationUser c))


-- | Shortcuts for typeclasses
class (ConnectionManager (Pool (DefaultDatabase g)) (DefaultDatabase g), PersistBackend (DbPersist (DefaultDatabase g) (WhebT g (WhebState g) IO))) => HasDB g where
instance WhebApp g => HasDB g where

instance (WhebApp g) => SessionApp g where
    type SessionAppBackend g = DefaultSession g
    getSessionContainer = getSessionBackend


class (HasDB g, SessionApp g) => WhebApp g where

    type DefaultRegistration g :: *

    type WhebUser g :: *
    type WhebUser g = RegistrationUser (DefaultRegistration g)

    type DefaultDatabase g :: *
    type DefaultSession g :: *
    type WhebState g :: *

    getCurrentUser :: WhebState g -> Maybe (WhebUser g)
    modifyAuthUser :: (Maybe (WhebUser g) -> Maybe (WhebUser g)) -> WhebState g -> WhebState g

    getPersistApp  :: g -> Pool (DefaultDatabase g)
    getRegistrationBackend :: (RegistrationBackend (DefaultRegistration g)) => g -> DefaultRegistration g
    getSessionBackend :: g -> DefaultSession g

data SimpleReg = SimpleReg

data MyUser =
    MyUser { name :: String
           , regToken :: String
           , password :: String
           , isActive :: Bool
           } deriving (Show)

mkPersist defaultCodegenConfig [groundhog|
- entity: MyUser               # Name of the datatype
  constructors:
    - name: MyUser
      fields:
        - name: name
        - name: regToken
        - name: isActive
|]


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


