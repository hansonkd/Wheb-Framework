{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies, RecordWildCards, Rank2Types, MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Core as GC
import Database.Groundhog.Expression
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Migration hiding (MigrationPack(..))
import qualified Database.Groundhog.Generic.Migration as GM

import Database.Groundhog.TH
import Database.Groundhog.Core (Unique(..))

import qualified Database.Groundhog.Generic.PersistBackendHelpers as H

import Data.String (fromString)
import Control.Arrow ((***))
import Control.Monad (liftM, forM)
import Control.Monad.Logger (MonadLogger, NoLoggingT, logDebugS)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask, Reader(..))
import qualified Data.ByteString.Char8 as BS
import Data.Char (toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intercalate, isInfixOf, partition, sort)
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromJust, catMaybes)
import Data.Either (rights)
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Database.RethinkDB as R hiding (Update)
import           Database.RethinkDB.NoClash hiding (runOpts, tableName, replace, update, delete, insert, get, Update(Update))
import           Database.RethinkDB.Datum (resultToMaybe)
import           Database.RethinkDB.ReQL (reqlToDatum)

data Rethink = Rethink (RethinkDBHandle)

instance R.Expr PersistValue where
    expr (PersistString s) = expr s
    expr (PersistByteString s) = expr s
    expr (PersistInt64 s) = expr s
    expr (PersistDouble s) = expr s
    expr (PersistBool s) = expr s
    expr (PersistUTCTime s) = expr s
    expr (PersistZonedTime (ZT s)) = expr s
    expr (PersistNull) = expr R.Null
    expr (PersistCustom s _) = expr R.Null

instance ToDatum PersistValue where
    toDatum (PersistString s) = toDatum s
    toDatum (PersistByteString s) = toDatum s
    toDatum (PersistInt64 s) = toDatum s
    toDatum (PersistDouble s) = toDatum s
    toDatum (PersistBool s) = toDatum s
    toDatum (PersistUTCTime s) = toDatum s
    toDatum (PersistZonedTime (ZT s)) = toDatum s
    toDatum (PersistNull) = toDatum R.Null
    toDatum (PersistCustom s _) = toDatum R.Null

instance FromDatum PersistValue where
  parseDatum (R.Null) = return $ PersistNull
  parseDatum (R.Bool b) = return $ PersistBool b
  parseDatum (R.String s) = return $ (PersistByteString $ T.encodeUtf8 s)
  parseDatum (R.Number n) = return $ PersistDouble n
  parseDatum (R.Array _) = return $ PersistNull
  parseDatum (R.Time zt) = return $ PersistZonedTime (ZT zt)
  parseDatum (R.Point _) = return $ PersistNull
  parseDatum (R.Object _) = return $ PersistNull
  parseDatum (R.Line _) = return $ PersistNull
  parseDatum (R.Polygon _) = return $  PersistNull
  parseDatum (R.Binary b) = return $ PersistByteString b
    
instance PrimitivePersistField Datum where
  toPrimitivePersistValue _ a = datumToPersist a

  fromPrimitivePersistValue _ = toDatum
 
instance PersistField Datum where
  persistName _ = "Datum"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbBlob False Nothing Nothing


newtype RethinkReQL r a = RethinkReQL {runRethink :: r}

instance DbDescriptor Rethink where
  type AutoKeyType Rethink = T.Text
  type QueryRaw Rethink = RethinkReQL ReQL
  backendName _ = "rethinkdb"

instance (MonadBaseControl IO m, MonadIO m, MonadLogger m) => PersistBackend (DbPersist Rethink m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Rethink (NoLoggingT IO)) #-}
  type PhantomDb (DbPersist Rethink m) = Rethink
  insert = insert'
  insert_ = insert_'
  insertBy = insertBy' -- insertBy' u v
  insertByAll = insertByAll' -- insertByAll' v
  replace = replace' -- replace' k v
  replaceBy = replaceBy' -- replaceBy' k v
  select = select' -- select' options
  selectAll = selectAll' -- undefined -- selectAll'
  get = get' -- get' k
  getBy = getBy' -- getBy' k
  update = update' -- update' upds cond
  delete = delete' -- delete' cond
  deleteBy = deleteBy' -- deleteBy' k
  deleteAll = deleteAll' -- deleteAll' v
  count = count' -- count' cond
  countAll = countAll' -- countAll' fakeV
  project p options = undefined -- project' p options
  migrate = migrate'

  executeRaw False query ps = undefined
  executeRaw True query ps = undefined
  queryRaw False query ps f = undefined
  queryRaw True query ps f = undefined

  insertList l = undefined -- insertList' l
  getList k = undefined -- getList' k

migrate' :: forall m v . (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v) => v -> Migration (DbPersist Rethink m)
migrate' v = runConst
    where e = entityDef proxy v
          proxy = undefined :: proxy (Rethink)
          (constructorNum, uniques) = getUniques proxy v
          constr = constructors e !! constructorNum
          uniqueDefs = constrUniques constr
          tableN = (T.pack $ entityName e)
          runConst = do
              return ()
          
datumToPersist :: Datum -> PersistValue
datumToPersist a = fromJust $ resultToMaybe (fromDatum a)

withRethinkPool :: (MonadBaseControl IO m, MonadIO m)
               => String -- ^ connection string
               -> Integer -- ^ connection port
               -> Maybe String -- ^ connection password
               -> Int -- ^ number of connections to open
               -> (Pool Rethink -> m a)
               -> m a
withRethinkPool s p pass connCount f = createRethinkPool s p pass connCount >>= f

withRethinkConn :: (MonadBaseControl IO m, MonadIO m)
               => String -- ^ connection string
               -> Integer -- ^ connection port
               -> Maybe String -- ^ connection password
               -> (Rethink -> m a)
               -> m a
withRethinkConn s p pass = bracket (liftIO $ open' s p pass) (liftIO . close')

createRethinkPool :: MonadIO m
                 => String -- ^ connection string
                 -> Integer -- ^ connection port
                 -> Maybe String -- ^ connection password
                 -> Int -- ^ number of connections to open
                 -> m (Pool Rethink)
createRethinkPool s p pass connCount = liftIO $ createPool (open' s p pass) close' 1 20 connCount

instance ConnectionManager Rethink Rethink where
  withConn f conn = f conn
  withConnNoTransaction f conn = f conn

instance ConnectionManager (Pool Rethink) Rethink where
  withConn f pconn = withResource pconn (withConn f)
  withConnNoTransaction f pconn = withResource pconn (withConnNoTransaction f)

instance SingleConnectionManager Rethink Rethink

open' :: String -> Integer -> Maybe String -> IO Rethink
open' host port pass = do
  conn <- R.connect host port pass
  return $ Rethink conn

close' :: Rethink -> IO ()
close' (Rethink conn) = R.close conn

insertCommon' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m, Result r) => v -> DbPersist Rethink m r
insertCommon' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef proxy v
      constructorNum = fromPrimitivePersistValue proxy (head vals) 
      constr = if isSimple (constructors e)
        then head $ constructors e
        else constructors e !! constructorNum
      rql = insertIntoConstructorTable (T.pack $ entityName e) constr (tail vals) 
  executeRaw' rql

insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => v -> DbPersist Rethink m (AutoKey v)
insert' v = do
    wr <- insertCommon' v
    liftM fst $ pureFromPersistValue $ map (PersistString . T.unpack) (maybe [] id $ writeResponseGeneratedKeys wr)

insert_' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => v -> DbPersist Rethink m ()
insert_' = insertCommon'

insertBy' :: forall u v m . (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v, IsUniqueKey (Key v (Unique u))) => u (UniqueMarker v) -> v -> DbPersist Rethink m (Either (AutoKey v) (AutoKey v))
insertBy' k v = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          u = (undefined :: u (UniqueMarker v) -> u (UniqueMarker v)) k
          indexN = flattenChain (fieldChain proxy u) []
          constr = head $ constructors e
          runConst = do
            uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
            ent <- executeRaw' $ table tableN # R.getAll (R.Index $ head indexN) (map (expr . toDatum) $ (uniques []))
            maybe (liftM Right $ insert v) (return . Left . fst . fromPurePersistValues proxy . (flip (:) []) . PersistString) $ ent >>= M.lookup (maybe "id" T.pack $ constrAutoKeyName constr)

insertByAll' :: forall m v . (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v) => v -> DbPersist Rethink m (Either (AutoKey v) (AutoKey v))
insertByAll' v = runConst
    where e = entityDef proxy v
          proxy = undefined :: proxy (Rethink)
          (constructorNum, uniques) = getUniques proxy v
          constr = constructors e !! constructorNum
          uniqueDefs = constrUniques constr
          tableN = (T.pack $ entityName e)
          conds :: ReQL -> ReQL
          conds a = let group = concat $ zipWith (\uFields (_, uVals) -> zipWith (\e v -> a!e R.== (expr v)) uFields (uVals [])) (mapMaybe f uniqueDefs) (uniques)
                    in case group of
                        [] -> (expr False)
                        _ -> foldl1 (\a b -> a R.&& b) group
          f u@(UniqueDef _ _ uFields) = if null $ rights uFields
                   then Just $ map expr $ (foldr flatten [] $ getUniqueFields u :: [T.Text])
                   else Nothing
          idField = maybe "id" T.pack $ constrAutoKeyName constr
          runConst = do
              c <- executeRaw' $ table tableN # R.filter (\a -> conds a) # (!0) # (!(expr idField))
              case (c :: Maybe (Datum)) of
                  Just d -> return $ Left $ fst $ fromPurePersistValues proxy $ [datumToPersist d]
                  Nothing -> liftM (Right) $ insert v
get' :: forall m v . 
        (PersistEntity v,
         MonadIO m,
         MonadBaseControl IO m, 
         MonadLogger m,
         PrimitivePersistField (Key v BackendSpecific)
        ) => Key v BackendSpecific -> DbPersist Rethink m (Maybe v)
get' k = runConst
  where e = entityDef proxy (undefined :: v)
        proxy = undefined :: proxy (Rethink)
        tableN = (T.pack $ entityName e)
        constr = head $ constructors e
        runConst = do
            ent <- getAll' (R.PrimaryKey) [toPrimitivePersistValue proxy k] tableN
            maybe (return Nothing) (fmap (Just . snd) . createEntity e) (ent :: Maybe (M.Map T.Text Datum))

getBy' :: forall m v u . (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v, IsUniqueKey (Key v (Unique u))) => Key v (Unique u) -> DbPersist Rethink m (Maybe v)
getBy' k = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          constr = head $ constructors e
          u = (undefined :: Key v (Unique u) -> u (UniqueMarker v)) k
          indexN = flattenChain (fieldChain proxy u) []
          runConst = do
            uniques <- toPersistValues k
            ent <- getAll' (R.Index $ head indexN) (uniques []) tableN
            
            maybe (return Nothing) (fmap (Just . snd) . createEntity e) ent

flattenChain (f, prefix) acc = (case prefix of
    ((name, EmbeddedDef False _):fs) -> flattenP (goP (fromString name) fs) f acc
    _ -> flatten f acc)

goP p ((name, EmbeddedDef False _):fs) = goP (fromString name <> "." <> p) fs
goP p _ = p
          
getAll' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Index -> [PersistValue] -> T.Text -> DbPersist Rethink m (Maybe (M.Map T.Text Datum))
getAll' i v tableN = executeRaw' $ table tableN # R.getAll i (map (expr . toDatum) v) # (!0)

update' :: forall m r v c . (MonadBaseControl IO m, MonadIO m, MonadLogger m, r ~ RestrictionHolder v c, PersistEntity v, EntityConstr v c) => [Update Rethink r] -> Cond Rethink r -> DbPersist Rethink m ()
update' upds conds = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          runConst = executeRaw' $ (table tableN) # R.filter (\a -> createConds a conds) # R.update (\a -> (foldedUpdates))
          foldedUpdates = renderUpdates' upds
 
delete' :: forall m r v c . (MonadBaseControl IO m, MonadIO m, MonadLogger m, r ~ RestrictionHolder v c, PersistEntity v, EntityConstr v c) =>  Cond Rethink r -> DbPersist Rethink m ()
delete' conds = runConst
     where e = entityDef proxy (undefined :: v)
           proxy = undefined :: proxy (Rethink)
           tableN = (T.pack $ entityName e)
           runConst = executeRaw' $ (table tableN) # R.filter (\a -> createConds a conds) # R.delete 

deleteBy' :: forall m v u . (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific)) => Key v BackendSpecific -> DbPersist Rethink m ()
deleteBy' k = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          constr = head $ constructors e
          idName = R.PrimaryKey
          runConst = do
            uniques <- toPersistValues k
            executeRaw' $ table tableN # R.getAll (idName) (map (expr . toDatum) $ uniques []) # R.delete

deleteAll' :: forall m v . (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v) => v -> DbPersist Rethink m ()
deleteAll' _ = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          runConst = executeRaw' $ table tableN # R.delete

count' :: forall m r v c . (MonadBaseControl IO m, MonadIO m, MonadLogger m, r ~ RestrictionHolder v c, PersistEntity v, EntityConstr v c) =>  Cond Rethink r -> DbPersist Rethink m Int
count' conds = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          runConst = executeRaw' $ (table tableN) # R.filter (\a -> createConds a conds) # R.count

countAll' :: forall m r v c . (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v) =>  v -> DbPersist Rethink m Int
countAll' _ = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          runConst = executeRaw' $ (table tableN) # R.count
          
selectAll' :: forall m v . (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Rethink m [((AutoKey v), v)]
selectAll' = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          runConst = do
              datums <- executeRaw' $ table tableN
              mapM (createEntity e) datums

select' :: forall m db r v c opts . (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m,  EntityConstr v c, HasSelectOptions opts Rethink (RestrictionHolder v c)) => opts -> DbPersist Rethink m [v]
select' optsLike = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          opts = getSelectOptions optsLike
          conds = condOptions opts
          runConst = do
              datums <- executeRaw' $ (table tableN) # R.filter (\a -> createConds a conds)
              mapM (liftM snd . createEntity e) datums

replace' :: forall m r v. (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific)) => Key v BackendSpecific -> v -> DbPersist Rethink m ()
replace' k v = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          constr = head $ constructors e
          idName = R.PrimaryKey
          idField = maybe "id" T.pack $ constrAutoKeyName constr
          runConst = do
            vals <- toEntityPersistValues' v
            uniques <- toPersistValues k
            let renderedUniques = uniques []
            executeRaw' $ table tableN # R.getAll (idName) (map (expr . toDatum) $ renderedUniques) # R.replace (\a -> R.merge ([idField := a!(expr idField)]) $ constructorToReQL constr (tail vals))

replaceBy' :: forall m v u . (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v, IsUniqueKey (Key v (Unique u))) => u (UniqueMarker v) -> v  -> DbPersist Rethink m ()
replaceBy' k v = runConst
    where e = entityDef proxy (undefined :: v)
          proxy = undefined :: proxy (Rethink)
          tableN = (T.pack $ entityName e)
          u = (undefined :: u (UniqueMarker v) -> u (UniqueMarker v)) k
          indexN = flattenChain (fieldChain proxy u) []
          idField = maybe "id" T.pack $ constrAutoKeyName constr
          constr = head $ constructors e
          runConst = do
            vals <- toEntityPersistValues' v
            uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
            executeRaw' $ table tableN # R.getAll (R.Index $ head indexN) (map (expr . toDatum) $ (uniques [])) # R.replace (\a -> R.merge ([idField := a!(expr idField)]) $ constructorToReQL constr (tail vals))

insertIntoConstructorTable :: T.Text -> ConstructorDef -> [PersistValue] -> ReQL
insertIntoConstructorTable tName c vals = (table tName) # R.insert (constructorToReQL c vals)

constructorToReQL :: ConstructorDef -> [PersistValue] -> ReQL
constructorToReQL c vals = expr $ zipWith (:=) fields vals
  where fields = foldr flatten [] (constrParams c)

createConds :: ReQL ->  Cond Rethink r -> ReQL
createConds ent (And c1 c2) = (createConds ent c1) R.&& (createConds ent c2)
createConds ent (Or c1 c2) = (createConds ent c1) R.|| (createConds ent c2)
createConds ent (Not c1) = R.not (createConds ent c1)
createConds ent (Compare c c1 c2) = (fromExprRelation c) (renderExpr' ent c1) (renderExpr' ent c2)
createConds ent (CondRaw r) = runRethink r
createConds ent CondEmpty = R.empty

renderExpr' :: ReQL -> UntypedExpr Rethink r -> ReQL
renderExpr' ent expres = case expres of
  ExprRaw q -> runRethink q
  ExprPure a -> let vals = toPurePersistValues proxy a
                    proxy = undefined :: proxy (Rethink)
                 in R.expr $ toDatum $ head $ vals []
  ExprCond  a -> createConds ent a
  ExprField f -> ent!?(fromString $ (T.unpack . head) $ (flattenChain f) [])

renderExprUpdate' :: UntypedExpr Rethink r -> ReQL
renderExprUpdate' expres = case expres of
  ExprRaw q -> runRethink q
  ExprPure a -> let vals = toPurePersistValues proxy a
                    proxy = undefined :: proxy (Rethink)
                 in R.expr $ toDatum $ head $ vals []
  ExprCond  a -> createConds R.empty a
  ExprField f -> (fromString $ (T.unpack . head) $ (flattenChain f) [])

renderUpdateField (ExprField f) = head $ (flattenChain f) []

fromExprRelation :: ExprRelation -> (ReQL -> ReQL -> ReQL)
fromExprRelation Eq = (R.==)
fromExprRelation Ne = (R./=)
fromExprRelation Gt = (R.>)
fromExprRelation Lt = (R.<)
fromExprRelation Ge = (R.>=)
fromExprReltaion Le = (R.<=)

{-# INLINABLE renderUpdates' #-}
renderUpdates' :: forall r v c . (r ~ RestrictionHolder v c, PersistEntity v, EntityConstr v c) => [GC.Update Rethink r] -> ReQL
renderUpdates' upds = expr $ foldr go [] upds where
  go :: forall a r . GC.Update Rethink r -> [Attribute a] -> [Attribute a]
  go (GC.Update field expres) acc = acc <> [ fs := (rend expres)]  where
    rend = renderExprUpdate'
    fs = mconcat $ map (renderUpdateField) (projectionExprs field [] :: [UntypedExpr Rethink r]) :: T.Text
    
createEntity :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => EntityDef -> M.Map T.Text Datum -> DbPersist Rethink m ((AutoKey v), v)
createEntity e datum = do
  let constrId = maybe 0 id $ M.lookup "desc" datum >>= resultToMaybe . fromDatum
      constr = constructors e !! constrId
      fields = (maybe "id" T.pack (constrAutoKeyName constr)):(foldr flatten [] (constrParams constr))
  mkEntity proxy constrId $ map (\f -> datumToPersist $ fromJust $ M.lookup f datum) fields

  
-- | receives constructor number and row of values from the constructor table
mkEntity :: (PersistEntity v, PersistBackend m) => proxy (PhantomDb m) -> Int -> [PersistValue] -> m (AutoKey v, v)
mkEntity proxy i xs = fromEntityPersistValues (toPrimitivePersistValue proxy i:xs') >>= \(v, _) -> return (k, v)
    where (k, xs') = fromPurePersistValues proxy xs

getLastInsertRowId :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Rethink m PersistValue
getLastInsertRowId = undefined

getStatement _ = undefined
getStatementCached _ = undefined

executeRaw' :: (Show query, R.Expr query, MonadBaseControl IO m, MonadIO m, MonadLogger m, Result r) => query -> DbPersist Rethink m r
executeRaw' query = do
  $logDebugS "ReQL" $ T.pack $ show query
  Rethink conn <- DbPersist ask
  liftIO $ R.run conn query

executeRawCached' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => ReQL -> DbPersist Rethink m ()
executeRawCached' query = do
  $logDebugS "ReQL" $ T.pack $ show query
  Rethink conn <- DbPersist ask
  liftIO $ R.run' conn query
  return ()

-- It is used to escape table names and columns, which can include only symbols allowed in Haskell datatypes and '$' delimiter. We need it mostly to support names that coincide with SQL keywords
escape :: String -> String
escape s = '\"' : s ++ "\""

escapeS :: T.Text -> T.Text
escapeS a = a

flatten :: (String, DbType) -> ([T.Text] -> [T.Text])
flatten (fname, typ) acc = go typ where
  go typ' = case typ' of
    DbEmbedded emb _ -> handleEmb emb
    _            -> fullName : acc
  fullName = fromString fname
  handleEmb (EmbeddedDef False ts) = foldr (flattenP fullName) acc ts
  handleEmb (EmbeddedDef True  ts) = foldr flatten acc ts

flattenP :: T.Text -> (String, DbType) -> ([T.Text] -> [T.Text])
flattenP prefix (fname, typ) acc = go typ where
  go typ' = case typ' of
    DbEmbedded emb _ -> handleEmb emb
    _            -> fullName : acc
  fullName = prefix <> "." <> fromString fname
  handleEmb (EmbeddedDef False ts) = foldr (flattenP fullName) acc ts
  handleEmb (EmbeddedDef True  ts) = foldr flatten acc ts

proxy :: proxy Rethink
proxy = error "proxy Rethink"

toEntityPersistValues' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v) => v -> DbPersist Rethink m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

data Machine = Machine { modelName :: String, cost :: Double } deriving Show
data Part = Part { partName :: String, weight :: Int, machine :: DefaultKey Machine }
deriving instance Show Part

mkPersist defaultCodegenConfig [groundhog|
- entity: Machine
  keys:
      - name: NameConstraint
  constructors:
    - name: Machine
      fields:
        - name: modelName
        - name: cost
      uniques:
        - name: NameConstraint
          fields: [modelName]
- entity: Part
|]

main = withRethinkPool "127.0.0.1" 28015 Nothing 5 $ runDbConn $ do
  executeRawCached' $ tableDrop (table "Machine")
  executeRawCached' $ tableCreate (table "Machine")
  
  megatron <- insert $ Machine "Megatron 5000" 2500.00
  megatron2 <- insert $ Machine "Megatron 1000" 1000.00
  
  megatron3 <- insertByAll $ Machine "Megatron 1000" 1000.00
  megatron4 <- insertByAll $ Machine "Megatron 3000" 1000.00
  
  replace megatron2  $ Machine "Megatron changed" 1000.00
  
  prereplaced <- get megatron2
  
  liftIO $ do
      print megatron
      
      print (megatron2)
      print (megatron3)
      print (megatron4)
      
      print (prereplaced)
