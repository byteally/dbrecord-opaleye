{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeFamilyDependencies, UndecidableInstances, ExplicitForAll, TypeApplications, ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, DeriveGeneric, Arrows, TypeOperators, FlexibleInstances, GeneralizedNewtypeDeriving, GADTs, PartialTypeSignatures #-}
-- | 

module Opaleye.Record where

import Opaleye hiding (Column, Table, leftJoin, aggregate, literalColumn)
import qualified Opaleye as O
import Opaleye.Internal.TableMaker (ColumnMaker)
import Opaleye.Internal.Join (NullMaker)
import Database.Migration hiding (Column, def, Col)
import qualified Database.Migration as Mig
import Database.Transaction
import Database.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.UUID.Types
import Data.Aeson
import Data.Int
import           Data.CaseInsensitive  (CI)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8          as ASCII
import Data.Functor.Const
import Data.Functor.Identity
import Data.Profunctor
import Data.Functor.Contravariant (contramap)
import Data.Profunctor.Product.Default
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import GHC.Generics
import Data.Proxy
import Control.Arrow ((>>^))
import GHC.TypeLits
import qualified Opaleye.Manipulation as M (runDelete, runInsert, runInsertReturning, runUpdate, runUpdateReturning)
import qualified Opaleye.RunQuery     as M (runQuery)
import Control.Monad.Reader
import Data.Coerce
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError(..), typename, returnError)
import Data.Typeable
import Data.List (find)
import Text.Read
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import Opaleye.Internal.PGTypes (literalColumn)


data Op t
newtype Hask t = Hask t
data HaskW' db (tab :: (* -> *) -> *) t
type HaskW db (tab :: (* -> *) -> *) = tab (HaskW' db tab)
data Write' (f :: * -> *) db (tab :: (* -> *) -> *) t
type Write (f :: * -> *) db (tab :: (* -> *) -> *) = tab (Write' f db tab)
data LJoin (f :: * -> *) t
type instance Column Op t   = O.Column (OpTypeRep t)
type instance Column Hask t = Hask t
type VoidF = Const ()

type family Col f (cn :: Symbol) (t :: *) = (r :: *) where
  Col Op   cn r            = O.Column (OpTypeRep r)
  Col Hask cn r            = r
  Col (HaskW' db tab) cn r = MkDefFldOpt (Elem (HasDefault (tab Hask)) cn) r
  Col (Write' f db tab) cn r = MkDefFldOpt (Elem (HasDefault (tab Hask)) cn) (Col f cn r)
  Col (Prj f flds) cn r    = ColSelect (Elem flds cn) (Col f cn r)
  Col (LJoin f) cn r       = Col f cn (Maybe r)
  Col (Agg f aggs) cn r    = LookupAgg f cn r aggs

type family ColSelect (isDroped :: Bool) (col :: *) :: * where
  ColSelect 'True col  = col
  ColSelect 'False col = VoidF col

type family MkDefFldOpt (hasDef :: Bool) (ft :: *) where
  MkDefFldOpt 'True ft         = Maybe ft
  MkDefFldOpt 'False ft        = ft

type family FlattenMaybe (t :: *) :: * where
  FlattenMaybe (Maybe (Maybe t)) = Maybe t
  FlattenMaybe mt                = mt

type family LookupAgg (f :: * -> *) (col :: Symbol) (t :: *) (aggs :: [AggFn]) :: * where
  LookupAgg f cn r ((fn (cn :: Symbol)) ': _)     = Col f cn (AggRes (fn cn) r)
  LookupAgg f cn r ((fn (cn1 :: Symbol)) ': aggs) = LookupAgg f cn r aggs
  LookupAgg f cn r '[]                           = ColSelect 'False (Col f cn r)

type family ElemAgg (col :: Symbol) (aggs :: [AggFn]) :: Bool where
  ElemAgg cn ((fn (cn :: Symbol)) ': _)   = 'True
  ElemAgg _ _                            = 'False

type family AggRes (fn :: AggFn) (t :: *) :: * where
  AggRes ('GroupBy _) r   = r
  AggRes ('Sum _) r       = r
  AggRes ('Count _) r     = Int64
  AggRes ('Avg _) r       = r
  AggRes ('Max _) r       = r
  AggRes ('Min _) r       = r
  AggRes ('BoolOr _) r    = r
  AggRes ('BoolAnd _) r   = r
  AggRes ('ArrayAgg _) r  = Vector r
  AggRes ('StringAgg _) r = r

{- Missing mapping
--  OpTypeRep Float         = PGFloat8
--  OpTypeRep Int32         = PGInt4
--  OpTypeRep String        = PGText
--  OpTypeRep [t]           = PGArray (OpTypeRep t)
--  OpTypeRep t             = CustomOpTypeRep t
-}

type family OpTypeRep (t :: *) = (r :: *) | r -> t where
  OpTypeRep Int           = PGInt4
  OpTypeRep Int16         = PGInt2
  OpTypeRep Int64         = PGInt8
  OpTypeRep Double        = PGFloat8
  OpTypeRep Integer       = PGNumeric
  OpTypeRep Text          = PGText
  OpTypeRep (CI Text)     = PGCitext
  OpTypeRep ByteString    = PGBytea
  OpTypeRep Bool          = PGBool
  OpTypeRep Day           = PGDate
  OpTypeRep UTCTime       = PGTimestamptz
  OpTypeRep LocalTime     = PGTimestamp
  OpTypeRep TimeOfDay     = PGTime
  OpTypeRep Value         = PGJsonb
  OpTypeRep (Json a)      = PGCustom (Json a) PGJsonb
  OpTypeRep (JsonStr a)   = PGCustom (JsonStr a) PGJson
  OpTypeRep UUID          = PGUuid
  OpTypeRep (Maybe t)     = Nullable (OpTypeRep t)
  OpTypeRep (Vector t)    = PGArray (OpTypeRep t)
  OpTypeRep a             = PGCustom a (GetPGTypeK (GetPGTypeRep a) (GetTypeFields a))

data PGCustom a pg
data OpRep (n :: PGTypeK)
data NTOpRep (t :: *) (rep :: *)
data EnumRep (pgK :: PGTypeK) (cons :: [Symbol])

type family GetPGTypeK (pgK :: PGTypeK) (flds :: [(Symbol, [*])]) = (r :: *) where
  GetPGTypeK ('PGCustomType a pgK 'True) _ = NTOpRep (InnerTy a) (OpTypeRep (InnerTy a))
  GetPGTypeK ('PGCustomType a pgK 'False) '[ '(_,_)] = OpRep pgK
  GetPGTypeK ('PGCustomType a pgK 'False) flds       = EnumRep pgK (GetEnumCons flds)

type family GetEnumCons (flds :: [(Symbol, [*])]) :: [Symbol] where
  GetEnumCons ('(c, _) ': cons) = c ': GetEnumCons cons
  GetEnumCons '[]                = '[]
                           
data Tab (db :: *) (tab :: (* -> *) -> *) = Tab

type family GetOpFields (f :: * -> *) (fs :: [*]) :: [*] where
  GetOpFields f ((fn ::: t) ': fs) = (fn ::: Col f fn t) ': GetOpFields f fs
  GetOpFields f '[]                = '[]

queryRec :: forall db tab flds opFlds.
           ( Table db (tab Hask)
           , Default ColumnMaker (HList Identity opFlds) (HList Identity opFlds)
           , flds ~ (GetTableFields (tab Hask))
           , opFlds ~ GetOpFields Op flds
           , TabProps2 db tab flds
           ) => Tab db tab -> Query (HList Identity opFlds)
queryRec tab =
  let cols = singCols (Proxy @db) (Proxy @(GetTableFields (tab Hask)))
  in queryTable (O.Table (T.unpack $ getConst $ getTableName @db @(tab Hask)) (tabProps2 tab cols))

query :: forall db tab flds opFlds.
        ( Table db (tab Hask)
        , Default ColumnMaker (HList Identity opFlds) (HList Identity opFlds)
        , flds ~ (GetTableFields (tab Hask))
        , opFlds ~ GetOpFields Op flds
        , TabProps2 db tab flds
        , Generic (tab Op)
        , GRecToType Identity (Rep (tab Op)) opFlds
        ) => Tab db tab -> Query (tab Op)
query tab =
  let cols = singCols (Proxy @db) (Proxy @(GetTableFields (tab Hask)))
  in queryTable (O.Table (T.unpack $ getConst $ getTableName @db @(tab Hask)) (tabProps2 tab cols)) >>^ recToType


type family Not (t :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

class TabProps2 db (tab :: (* -> *) -> *) (flds:: [*]) where
  tabProps2 :: Tab db tab -> HList (Const Mig.Column) flds -> TableProperties (HList Identity (GetOpFields (Write' Op db tab) flds)) (HList Identity (GetOpFields Op flds))

instance ( TabProps2 db tab flds
         , hasDef ~ Elem (HasDefault (tab Hask)) fn
         , (WriteSpec (Not hasDef) t) ~ (MkDefFldOpt hasDef (O.Column (OpTypeRep t)))
         , TabProps (Not hasDef) t
         ) => TabProps2 db tab ((fn ::: t) ': flds) where
  tabProps2 pxyDB ((Const (Mig.Column cn _)) :& cols)
    = let prop = tabProps (Proxy @ (Not hasDef)) (T.unpack cn)
      in (:&) <$> dimap (valOf . runIdentity . headRec) (Identity . Field) prop
         <*> lmap (tailRec) (tabProps2 pxyDB cols)

instance TabProps2 db tab '[] where
  tabProps2 _ Nil = pure Nil

class TabAgg (f :: * -> *) (aggs :: [AggFn]) (flds:: [*]) where
  tabAgg :: Proxy '(f, aggs) -> HList (Const Mig.Column) flds -> Aggregator (HList Identity (GetOpFields f flds)) (HList Identity (GetOpFields (Agg f aggs) flds))

instance ( TabAgg f aggs flds
         , AggSpec fn f t aggs (ElemAgg fn aggs)
         , (AggSpecRes fn f t aggs (ElemAgg fn aggs)) ~ (LookupAgg f fn t aggs)
         ) => TabAgg f aggs ((fn ::: t) ': flds) where
  tabAgg pagg ((Const (Mig.Column _ _)) :& cols)
    = let agg = aggSpec (Proxy @'(fn, f, t, aggs, ElemAgg fn aggs))
      in (:&) <$> dimap (valOf . runIdentity . headRec) (Identity . Field) agg
              <*> lmap (tailRec) (tabAgg pagg cols)
  
instance TabAgg f aggs '[] where
  tabAgg _ Nil = pure Nil

class AggSpec (cn :: Symbol) (f :: * -> *) (t :: *) (aggs :: [AggFn]) (isMat :: Bool) where
  type AggSpecRes cn f t aggs isMat :: *
  aggSpec :: Proxy '(cn, f, t, aggs, isMat) -> Aggregator (Col f cn t) (AggSpecRes cn f t aggs isMat)

instance ( Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('GroupBy cn ': aggs) 'True where
  type AggSpecRes cn f t ('GroupBy cn ': aggs) 'True = Col (Agg f ('GroupBy cn ': aggs)) cn t
  aggSpec _ = O.groupBy

instance ( Col f cn t ~ O.Column (OpTypeRep t)
         , t ~ Int64
         ) => AggSpec cn f t ('Count cn ': aggs) 'True where
  type AggSpecRes cn f t ('Count cn ': aggs) 'True = Col (Agg f ('Count cn ': aggs)) cn t
  aggSpec _ = O.count

-- TODO: Add a TypeError expressing t ~ Double
instance ( t ~ Double
         ,  Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('Avg cn ': aggs) 'True where
  type AggSpecRes cn f t ('Avg cn ': aggs) 'True = Col (Agg f ('Avg cn ': aggs)) cn t
  aggSpec _ = O.avg  

instance ( Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('Sum cn ': aggs) 'True where
  type AggSpecRes cn f t ('Sum cn ': aggs) 'True
    = Col (Agg f ('Sum cn ': aggs)) cn t
  aggSpec _ = O.sum

instance ( PGOrd (OpTypeRep t)
         , Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('Max cn ': aggs) 'True where
  type AggSpecRes cn f t ('Max cn ': aggs) 'True = Col (Agg f ('Max cn ': aggs)) cn t
  aggSpec _ = O.max

instance ( PGOrd (OpTypeRep t)
         , Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('Min cn ': aggs) 'True where
  type AggSpecRes cn f t ('Min cn ': aggs) 'True = Col (Agg f ('Min cn ': aggs)) cn t
  aggSpec _ = O.min

-- TODO: Add a TypeError expressing t ~ Bool
instance ( t ~ Bool
         , Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('BoolOr cn ': aggs) 'True where
  type AggSpecRes cn f t ('BoolOr cn ': aggs) 'True = Col (Agg f ('BoolOr cn ': aggs)) cn t
  aggSpec _ = O.boolOr

-- TODO: Add a TypeError expressing t ~ Bool
instance ( t ~ Bool
         , Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('BoolAnd cn ': aggs) 'True where
  type AggSpecRes cn f t ('BoolAnd cn ': aggs) 'True = Col (Agg f ('BoolAnd cn ': aggs)) cn t 
  aggSpec _ = O.boolOr  

instance ( Col f cn (Vector t) ~ O.Column (OpTypeRep (Vector t))
         , Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('ArrayAgg cn ': aggs) 'True where
  type AggSpecRes cn f t ('ArrayAgg cn ': aggs) 'True = Col (Agg f ('ArrayAgg cn ': aggs)) cn t
  aggSpec _ = O.arrayAgg

-- TODO: Add a TypeError expressing t ~ Text
instance ( t ~ Text
         , Col f cn t ~ O.Column (OpTypeRep t)
         ) => AggSpec cn f t ('StringAgg cn ': aggs) 'True where
  type AggSpecRes cn f t ('StringAgg cn ': aggs) 'True = Col (Agg f ('StringAgg cn ': aggs)) cn t 
  aggSpec _ = O.stringAgg (O.pgString "") -- TODO: Fix the default string
  
instance AggSpec cn f t aggs (ElemAgg cn aggs)
         => AggSpec cn f t (aggFn ': aggs) 'False where
  type AggSpecRes cn f t (aggFn ': aggs) 'False = AggSpecRes cn f t aggs (ElemAgg cn aggs)
  aggSpec _ = aggSpec (Proxy @'(cn, f, t, aggs, ElemAgg cn aggs))

instance AggSpec cn f t '[] isMat where
  type AggSpecRes cn f t '[] isMat = ColSelect 'False (Col f cn t)
  aggSpec _ = pure (Const ())
  
type family WriteSpec (isReq :: Bool) (t :: *) where
  WriteSpec 'True t  = O.Column (OpTypeRep t)
  WriteSpec 'False t = Maybe (O.Column (OpTypeRep t))

type ReadSpec (t :: *) = O.Column (OpTypeRep t)

class TabProps (isReq :: Bool) t where
  tabProps :: Proxy isReq -> String -> TableProperties (WriteSpec isReq t) (ReadSpec t)

instance TabProps 'True t where
  tabProps _ coln = required coln

instance TabProps 'False t where
  tabProps _ coln = optional coln

instance QueryRunnerColumnDefault (PGArray a) [b]
         => QueryRunnerColumnDefault (PGArray a) (Vector b) where
  queryRunnerColumnDefault = fmap V.fromList queryRunnerColumnDefault

instance ( Default Constant t (O.Column trep)
         , Coercible nt t
         , trep ~ OpTypeRep t
         ) => Default Constant nt (O.Column (PGCustom nt (NTOpRep t trep))) where
  def = Constant (toNewType . toOp . (coerce :: nt -> t))
    where toOp = constant :: t -> O.Column trep

instance (Coercible t nt, FromField t) => O.QueryRunnerColumnDefault (PGCustom nt (NTOpRep t trep)) nt where
  queryRunnerColumnDefault = fmap (coerce :: t -> nt) fieldQueryRunnerColumn 
  
fromNewType :: O.Column (PGCustom nt (NTOpRep t tr)) -> O.Column (OpTypeRep t)
fromNewType = O.unsafeCoerceColumn

toNewType :: O.Column (OpTypeRep t) -> O.Column (PGCustom nt (NTOpRep t tr))
toNewType = O.unsafeCoerceColumn

newtype PGEnum (pgK :: PGTypeK) (cons :: [Symbol]) a = PGEnum { getEnum :: a }

instance ( ShowPGType pgK
         , Typeable t
         , Typeable pgK
         , Typeable cons
         , All SingE cons
         , SingI cons
         , Read t
         ) => QueryRunnerColumnDefault (PGCustom t (EnumRep pgK cons)) t where
  queryRunnerColumnDefault = getEnum <$> fieldQueryRunnerColumn @(PGEnum pgK cons t)

instance ( Typeable t
         , Typeable pgK
         , Read t
         , ShowPGType pgK
         , Typeable cons
         , All SingE cons
         , SingI cons
         ) => FromField (PGEnum pgK cons t) where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just val) = do
    tName <- typename f
    let pgTy = Mig.showPGType (Proxy :: Proxy pgK)
        pgTyBS = encodeUtf8 pgTy
        haskTy = typeRep (Proxy @t)
        enumNames = fromSing (sing :: Sing cons)
    if tName == pgTyBS
      then case readMaybe =<< find (\ename -> val == ASCII.pack ename) enumNames of
            Just evalue -> return $ PGEnum evalue
            Nothing -> returnError ConversionFailed f (show val)
      else returnError Incompatible f ("Wrong database type for " ++ (show haskTy)
                                       ++ "expecting: " ++ (T.unpack pgTy) ++ ", saw: " ++ show tName)

instance Show t => Default Constant t (O.Column (PGCustom t (EnumRep pgK cons))) where
  def = Constant (literalColumn . HPQ.OtherLit . toPG)
    where toPG val = "'" ++ show val ++ "'"

instance ( Default Constant [a] (O.Column (PGArray arep))
         , arep ~ OpTypeRep a
         ) => Default Constant (Vector a) (O.Column (PGArray arep)) where
  def = Constant (constant . V.toList)

instance (ToJSON a) => Default Constant (Json a) (O.Column (PGCustom (Json a) PGJsonb)) where
  def = Constant (O.unsafeCoerceColumn . (constant :: Value -> O.Column PGJsonb) . toJSON . (coerce :: Json a -> a))

instance ( ToJSON a
         , FromJSON a
         ) => QueryRunnerColumnDefault (PGCustom (Json a) PGJsonb) (Json a) where
  queryRunnerColumnDefault = fmap (Mig.json . getResult . fromJSON) fieldQueryRunnerColumn
    where
      getResult (Success a) = a
      getResult (Error e)   = error $ show e


printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . maybe "Empty query" id . showSqlForPostgres


instance {-# OVERLAPPABLE #-} Default Constant a a where
  def = Constant id
  
instance {-# OVERLAPPABLE #-} Default Constant a (Maybe a) where
  def = Constant Just

instance (KnownSymbol tyName) => IsSqlType (PGCustom ty (EnumRep ('PGTypeName tyName) enums)) where
  showPGType _ = symbolVal (Proxy :: Proxy tyName)  



type family OpalResult (f :: * -> *) = (r :: * -> *) | r -> f where
  OpalResult Op            = Hask
  OpalResult (Prj f flds)  = Prj (OpalResult f) flds
  OpalResult (LJ f g)      = LJ (OpalResult f) (OpalResult g)
  OpalResult (Agg f aggs)  = Agg (OpalResult f) aggs

getAll :: ( Default QueryRunner (tab f) (tab (OpalResult f))
         , MonadPG m
         ) => Query (tab f) -> m [tab (OpalResult f)]
getAll q = liftPG $ do
  conn <- ask
  unsafeIOToTransaction $ runQuery conn q

getAll'
  :: ( Default QueryRunner columns haskells
    , MonadPG m
    ) => Query columns -> m [haskells]
getAll' q = liftPG  $ do
  conn <- ask
  unsafeIOToTransaction $ M.runQuery conn q

data LJTabs a b (f :: * -> *) where
  LJTabs :: tab1 f -> tab2 (LJoin g) -> LJTabs tab1 tab2 (LJ f g)

data LJ (f :: * -> *) (g :: * -> *) t

leftTab :: LJTabs tab1 tab2 (LJ f g) -> tab1 f
leftTab (LJTabs l _) = l

rightTab :: LJTabs tab1 tab2 (LJ f g) -> tab2 (LJoin g)
rightTab (LJTabs _ r) = r

instance ( Profunctor p
         , Applicative (p (LJTabs tab1 tab2 (LJ f1 g1)))
         , Default p (tab1 f1) (tab1 f2)
         , Default p (tab2 (LJoin g1)) (tab2 (LJoin g2))
         ) => Default p (LJTabs tab1 tab2 (LJ f1 g1)) (LJTabs tab1 tab2 (LJ f2 g2)) where
  def = LJTabs <$> lmap leftTab def
               <*> lmap rightTab def

leftJoin :: forall tab1 tab2 (f :: * -> *) (g :: * -> *).
           ( Default Unpackspec (tab1 f) (tab1 f)
           , Default Unpackspec (tab2 g) (tab2 g)
           , Default NullMaker (tab2 g) (tab2 (LJoin g))
           ) => Query (tab1 f) -> Query (tab2 g) -> ((tab1 f, tab2 g) -> ReadSpec Bool) -> Query (LJTabs tab1 tab2 (LJ f g))
leftJoin tab1 tab2 cond = O.leftJoin tab1 tab2 cond >>^ toLJTabs
  where toLJTabs (t1, t2) = LJTabs t1 t2

insert :: forall db tab m.
         ( Table db (tab Hask)
         , TabProps2 db tab (GenTabFields (Rep (tab Hask)))
         , GTypeToRec Identity (Rep (Write Op db tab)) (GetOpFields (Write' Op db tab) (GenTabFields (Rep (tab Hask))))
         , Generic (Write Op db tab)
         , Default Constant (HaskW db tab) (Write Op db tab)
         ) => MonadPG m => Tab db tab -> (HaskW db tab) -> m ()
insert tab row = liftPG $ do
  let cols = singCols (Proxy @db) (Proxy @(GetTableFields (tab Hask)))
      table = O.Table (T.unpack $ getConst $ getTableName @db @(tab Hask)) (tabProps2 tab cols)
  conn <- ask
  unsafeIOToTransaction . void $ M.runInsert conn table (typeToRec (constant row :: Write Op db tab))
delete :: forall db tab m.
         ( MonadPG m
         , Table db (tab Hask)
         , TabProps2 db tab (GenTabFields (Rep (tab Hask)))
         , Generic (tab Op)
         , GRecToType Identity (Rep (tab Op)) (GetOpFields Op (GenTabFields (Rep (tab Hask))))
         ) => Tab db tab -> (tab Op -> (ReadSpec Bool)) -> m Int64
delete tab cond = liftPG $ do
  let cols = singCols (Proxy @db) (Proxy @(GetTableFields (tab Hask)))
      table = O.Table (T.unpack $ getConst $ getTableName @db @(tab Hask)) (recToType <$> tabProps2 tab cols)
  conn <- ask
  unsafeIOToTransaction $ M.runDelete conn table cond

update :: forall db tab m.
         ( MonadPG m
         , Table db (tab Hask)
         , TabProps2 db tab (GenTabFields (Rep (tab Hask)))
         , Generic (tab Op)
         , Generic (Write Op db tab)
         , GRecToType Identity (Rep (tab Op)) (GetOpFields Op (GenTabFields (Rep (tab Hask))))
         , GTypeToRec Identity (Rep (Write Op db tab)) (GetOpFields (Write' Op db tab) (GenTabFields (Rep (tab Hask))))
         ) => Tab db tab -> (tab Op -> Write Op db tab) -> (tab Op -> ReadSpec Bool) -> m Int64
update tab upd cond = liftPG $ do
  let cols = singCols (Proxy @db) (Proxy @(GetTableFields (tab Hask)))
      table = O.Table (T.unpack $ getConst $ getTableName @db @(tab Hask)) (dimap typeToRec recToType $ tabProps2 tab cols)
  conn <- ask
  unsafeIOToTransaction $ M.runUpdate conn table upd cond

insertRet :: forall db tab m prjf.
            ( MonadPG m
            , Table db (tab Hask)
            , TabProps2 db tab (GenTabFields (Rep (tab Hask)))
            , GRecToType Identity (Rep (tab Op)) (GetOpFields Op (GenTabFields (Rep (tab Hask))))
            , GTypeToRec Identity (Rep (Write Op db tab)) (GetOpFields (Write' Op db tab) (GenTabFields (Rep (tab Hask))))
            , Generic (tab Op)
            , Generic (Write Op db tab)
            , Default QueryRunner (tab prjf) (tab (OpalResult prjf))
            , Default Constant (HaskW db tab) (Write Op db tab)
            ) => Tab db tab -> (HaskW db tab) -> (tab Op -> tab prjf) -> m [tab (OpalResult prjf)]
insertRet tab row ret = liftPG $ do
  let cols = singCols (Proxy @db) (Proxy @(GetTableFields (tab Hask)))
      table = O.Table (T.unpack $ getConst $ getTableName @db @(tab Hask)) (dimap typeToRec recToType $ tabProps2 tab cols)
  conn <- ask
  unsafeIOToTransaction $ M.runInsertReturning conn table (constant row :: Write Op db tab) ret

-- 
updateRet :: forall db tab m prjf.
            ( MonadPG m
            , Table db (tab Hask)
            , TabProps2 db tab (GenTabFields (Rep (tab Hask)))
            , Generic (tab Op)
            , Generic (Write Op db tab)
            , GTypeToRec Identity (Rep (Write Op db tab)) (GetOpFields (Write' Op db tab) (GenTabFields (Rep (tab Hask))))
            , GRecToType Identity (Rep (tab Op)) (GetOpFields Op (GenTabFields (Rep (tab Hask))))
            , Default QueryRunner (tab prjf) (tab (OpalResult prjf))
            ) => Tab db tab -> (tab Op -> Write Op db tab) -> (tab Op -> ReadSpec Bool) -> (tab Op -> tab prjf) -> m [tab (OpalResult prjf)]
updateRet tab upd cond ret = liftPG $ do
  let cols = singCols (Proxy @db) (Proxy @(GetTableFields (tab Hask)))
      table = O.Table (T.unpack $ getConst $ getTableName @db @(tab Hask)) (dimap typeToRec recToType $ tabProps2 tab cols)
  conn <- ask
  unsafeIOToTransaction $ M.runUpdateReturning conn table upd cond ret


data Prj (f :: * -> *) (flds :: [Symbol]) t

newtype ColProject col prjc = ColProject {toVoidCol :: col -> prjc}

instance Functor (ColProject col) where
  fmap f (ColProject g) = ColProject (fmap f g)

instance Applicative (ColProject col) where
  pure = ColProject . pure
  ColProject f <*> ColProject x = ColProject (f <*> x)

instance Profunctor ColProject where
  dimap f g (ColProject h) = ColProject (dimap f g h)

instance Default ColProject (O.Column c) (O.Column c) where
  def = ColProject id

instance Default ColProject (O.Column c) (VoidF (O.Column c)) where
  def = ColProject $ const (Const ())

instance ProductProfunctor ColProject where
  empty = PP.empty
  (***!) = PP.defaultProfunctorProduct

colProject :: Default ColProject col prjcol => col -> prjcol
colProject = toVoidCol def


project :: forall (flds :: [Symbol]) tab f.Default ColProject (tab f) (tab (Prj f flds)) => tab f -> tab (Prj f flds)
project tab = colProject tab

data Agg (f :: * -> *) (aggs :: [AggFn]) t

data AggFn
  = GroupBy Symbol
  | Sum Symbol
  | Count Symbol
  | Avg Symbol
  | Max Symbol
  | Min Symbol
  | BoolOr Symbol
  | BoolAnd Symbol
  | ArrayAgg Symbol
  | StringAgg Symbol

aggregate :: forall (aggs :: [AggFn]) tab f db flds.
            ( Generic (tab f)
            , Generic (tab (Agg f aggs))
            , GTypeToRec Identity (Rep (tab f)) (GetOpFields f flds)
            , GRecToType Identity (Rep (tab (Agg f aggs))) (GetOpFields (Agg f aggs) flds)
            , TabAgg f aggs flds
            , SingCols db flds
            , flds ~ (GetTableFields (tab Hask))
            ) => Tab db tab -> Query (tab f) -> Query (tab (Agg f aggs))
aggregate _ tab =
  let cols = singCols (Proxy @db) (Proxy @flds)
  in O.aggregate (tabAgg (Proxy @'(f, aggs)) cols) (tab >>^ typeToRec) >>^ recToType

aggregateOrdered :: forall (aggs :: [AggFn]) tab f db flds.
                   ( Generic (tab f)
                   , Generic (tab (Agg f aggs))
                   , GTypeToRec Identity (Rep (tab f)) (GetOpFields f flds)
                   , GRecToType Identity (Rep (tab f)) (GetOpFields f flds)
                   , GRecToType Identity (Rep (tab (Agg f aggs))) (GetOpFields (Agg f aggs) flds)
                   , TabAgg f aggs flds
                   , SingCols db flds
                   , flds ~ (GetTableFields (tab Hask))
                   ) => Tab db tab -> Order (tab f) -> Query (tab f) -> Query (tab (Agg f aggs))
aggregateOrdered _ ordBy tab =
  let cols = singCols (Proxy @db) (Proxy @flds)
  in O.aggregateOrdered (contramap recToType ordBy) (tabAgg (Proxy @'(f, aggs)) cols) (tab >>^ typeToRec) >>^ recToType     

-- column "User.id" must appear in the GROUP BY clause or be used in an aggregate function
--aggregate @'[Sum "a", GroupBy "b"] $ userQ


instance ( Profunctor p
         , Applicative (p (HList f (s ::: t1 : xs)))
         , Functor (p (s ::: t1))
         , Applicative f
         , Extract f
         , Default p t1 t2
         , Default p (HList f xs) (HList f ys)
         ) => Default p (HList f ((s ::: t1) ': xs)) (HList f ((s ::: t2) ': ys)) where
  def = (:&) <$> dimap (extract . headRec) pure def
             <*> lmap (tailRec) def

instance Applicative (p (HList f '[])) => Default p (HList f '[]) (HList f '[]) where
  def = pure Nil
  
instance ( Profunctor p
         , Default p t1 t2
         , Functor (p (s ::: t1))
         ) => Default p (s ::: t1) (s ::: t2) where
  def = Field <$> lmap valOf def


headRec :: HList f (x ': xs) -> f x
headRec (x :& _) = x

tailRec :: HList f (x ': xs) -> HList f xs
tailRec (_ :& xs) = xs
