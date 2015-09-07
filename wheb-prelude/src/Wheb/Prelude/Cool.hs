{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings, GADTs #-}
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

data Person = Person
    { personName :: !String
    , personAge :: !Int
    }
  deriving (Show)

type PersonId = Key Person

instance PersistEntity Person where
    newtype Key Person = PersonKey (BackendKey SqlBackend)
        deriving (PersistField, Show, Eq, Read, Ord)
    -- A Generalized Algebraic Datatype (GADT).
    -- This gives us a type-safe approach to matching fields with
    -- their datatypes.
    data EntityField Person typ where
        PersonId   :: EntityField Person PersonId
        PersonName :: EntityField Person String
        PersonAge  :: EntityField Person Int

    data Unique Person
    type PersistEntityBackend Person = SqlBackend

    toPersistFields (Person name age) =
        [ SomePersistField name
        , SomePersistField age
        ]

    fromPersistValues [nameValue, ageValue] = Personf
        <$> fromPersistValue nameValue
        <*> fromPersistValue ageValue
    fromPersistValues _ = Left "Invalid fromPersistValues input"

    -- Information on each field, used internally to generate SQL statements
    persistFieldDef PersonId = FieldDef
        (HaskellName "Id")
        (DBName "id")
        (FTTypeCon Nothing "PersonId")
        SqlInt64
        []
        True
        NoReference
    persistFieldDef PersonName = FieldDef
        (HaskellName "name")
        (DBName "name")
        (FTTypeCon Nothing "String")
        SqlString
        []
        True
        NoReference
    persistFieldDef PersonAge = FieldDef
        (HaskellName "age")
        (DBName "age")
        (FTTypeCon Nothing "Int")
        SqlInt64
        []
        True
        NoReference