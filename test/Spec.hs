{-# LANGUAGE DataKinds, Arrows, TypeApplications, PartialTypeSignatures, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, DeriveGeneric, GeneralizedNewtypeDeriving, DuplicateRecordFields #-}

import Opaleye hiding (Column, Table, leftJoin, aggregate)
import qualified Opaleye as O
import Opaleye.Constant
import Opaleye.DBRecord hiding (def)
import Data.Aeson
import Data.Profunctor.Product.Default
import Control.Monad.Reader
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Profunctor

data TestDB

newtype Age = Age {getAge :: Int}
  deriving (Generic)

newtype UserId = UserId {getUserId :: Int}
  deriving (Generic)

newtype AddressId = AddressId {getAddressId :: Int}
  deriving (Generic)

data Gender = Male | Female | Other
  deriving (Generic, Show, Read)  


data User f = User
  { user_id :: Col f "user_id" UserId
  , name    :: Col f "name" Text
  , age     :: Col f "age" Age
  , gender  :: Col f "gender" Gender
  , address_id :: Col f "address_id" AddressId
 } deriving (Generic)

data Address f = Address 
 { address_id  :: Col f "address_id" AddressId
 , street_1    :: Col f "street_1" Text
 , street_2    :: Col f "street_2" (Maybe Text)
 , city        :: Col f "city" Text
 , postal_code :: Col f "postal_code" Text

} deriving (Generic)

-- Due to lack of type inference for duplicated record fields,
-- we write a getter 

addr'AddressId :: Address f -> Col f "address_id" AddressId
addr'AddressId = address_id

user'AddressId :: User f -> Col f "address_id" AddressId
user'AddressId = address_id

-- TODO : TH function to automate the below definition
instance ( Profunctor p
         , Applicative (p (User f))
         , Default p (Col f "user_id" UserId) (Col g "user_id" UserId)
         , Default p (Col f "name" Text) (Col g "name" Text)
         , Default p (Col f "age" Age) (Col g "age" Age)
         , Default p (Col f "gender" Gender) (Col g "gender" Gender)
         , Default p (Col f "address_id" AddressId) (Col g "address_id" AddressId)
         ) => Default p (User f) (User g) where
  def = User <$> lmap user_id def
             <*> lmap name def
             <*> lmap age def
             <*> lmap gender def
             <*> lmap user'AddressId  def

instance ( Profunctor p
         , Applicative (p (Address f))
         , Default p (Col f "address_id" AddressId) (Col g "address_id" AddressId)
         , Default p (Col f "street_1" Text) (Col g "street_1" Text)
         , Default p (Col f "street_2" (Maybe Text)) (Col g "street_2" (Maybe Text))
         , Default p (Col f "city" Text) (Col g "city" Text)
         , Default p (Col f "postal_code" Text) (Col g "postal_code" Text)
         ) => Default p (Address f) (Address g) where
  def = Address <$> lmap addr'AddressId def
                <*> lmap street_1 def
                <*> lmap street_2 def
                <*> lmap city def
                <*> lmap postal_code def

  
instance Database TestDB where
  type DB TestDB     = 'Postgres
  type Tables TestDB = '[User Hask, Address Hask]
  type Types TestDB  = '[Gender]

instance Table TestDB (User Hask) where
  type HasDefault (User Hask) = '["user_id"]

instance Table TestDB (Address Hask) where
  type TableName (Address Hask) = "address"
  type ColumnNames (Address Hask) = '[ '("postal_code", "zip code")
                                     ]



-- A simple straight forward query on the user table 
-- would give us (User Op)
userQ :: Query (User Op)
userQ = proc () -> do
  u <- query (Tab @TestDB @User) -< ()
  restrict -< name u .== constant (T.pack "brian")
  restrict -< age u .== constant (Age 21)
  returnA -< u

-- Let's select project just the name and age
-- userPrjQ :: QueryArr () (User (Prj Op '["age", "name"]))
userPrjQ = proc () -> do
  u <- query (Tab @TestDB @User) -< ()
  returnA -< project @'["age", "name"] u

-- Project just name from the above query userPrjQ
-- QueryArr () (User (Prj (Prj Op '["age", "name"]) '["name"]))
userPrjQ1 = proc () -> do
  u <- userPrjQ -< ()
  returnA -< project @'["name"] u

-- Moving to the Hask layer from Op layer using getAll

-- getAll on UserQ would transform (User Op) to User Hask
userPG :: PG [User Hask]
userPG = getAll userQ

userPrj :: PG _ -- [User (Prj Hask '["age", "name"])]
userPrj = getAll userPrjQ

userTrans :: ReaderT (Config a) IO [User Hask]
userTrans = runTransaction userPG

userPGIO :: ReaderT (Config a) PG [User Hask]
userPGIO = getAll userQ


-- Left Join examples
------------------

addressQ :: Query (Address Op)
addressQ = proc () -> do
  add <- query (Tab @TestDB @Address) -< ()
  returnA -< add


-- userLJoinQ :: Query (LJTabs User Address (LJ Op Op))
userLJoinQ = leftJoin userQ addressQ ( (\(u, a) -> user'AddressId u .==  addr'AddressId a))

-- NOTE : The below where condition would give compilation error as we didnt project address ID
-- userLJoinQ' = leftJoin userPrjQ addressQ  ((\(u, a) -> user'AddressId u .==  addr'AddressId a))

-- userLJoinQ' :: Query (LJTabs User Address (LJ (Prj Op '["age", "name"]) Op))
userLJoinQ' = leftJoin userPrjQ addressQ (const $ (constant True :: ReadSpec Bool))


-- userLJoinQ'' :: Query (LJTabs User User (LJ Op (Prj Op '["age", "name"])))
userLJoinQ'' = leftJoin userQ userPrjQ (const $ (constant True :: ReadSpec Bool))


-- userLJPrjQ :: QueryArr () (User (Prj Op '["age", "name"]))
userLJPrjQ = proc () -> do
  ljres <- userLJoinQ' -< ()
  returnA -< leftTab ljres

-- userLJPrjQ' :: QueryArr () (User (Prj Op '["age", "name"]))
userLJPrjQ' = proc () -> do
  u <- userPrjQ -< ()
  ljres <- userLJoinQ' -< ()
  returnA -< leftTab ljres  

-- userLJoin :: ReaderT (Config a) PG [LJTabs User Address (LJ Hask Hask)]
userLJoin = getAll userLJoinQ :: ReaderT (Config a) PG _


-- Aggregation
-----------

-- userAggQ1 :: Query (User (Agg Op '['Sum "user_id", 'GroupBy "name"]))
userAggQ1 = aggregate @'[Sum "user_id", GroupBy "name"] (Tab @TestDB @User) userQ

-- userAggQ2 :: Query (User (Agg (Prj Op '["age", "name"]) '['Sum "age", 'GroupBy "name"]))
userAggQ2 = aggregate @'[Sum "age", GroupBy "name"] (Tab @TestDB @User) userPrjQ

-- userAggQ3 :: Query (User (Agg Op '['Sum "user_id", 'Sum "age", 'GroupBy "name"]))
userAggQ3 = Opaleye.DBRecord.aggregateOrdered @'[Sum "user_id", Sum "age", GroupBy "name"] (Tab @TestDB @User) (asc name) userQ

-- userAggQ4 :: Query (User (Agg (Prj (Prj Op '["age", "name"]) '["name"]) '['GroupBy "name"]))
userAggQ4 = aggregate @'[GroupBy "name"] (Tab @TestDB @User) userPrjQ1

-- userAggQ5 :: Query (User (Agg (Prj Op '["age", "name"]) '['GroupBy "name"]))
userAggQ5 = aggregate @'[GroupBy "name"] (Tab @TestDB @User) userLJPrjQ'

-- userAgg :: ReaderT (Config a) PG [User (Agg Hask '['Sum "user_id", 'GroupBy "name"])]
userAgg = getAll userAggQ1 :: ReaderT (Config a) PG _


-- Insert, Update, Delete
--------------------------

-- Note that we can send Nothing in place of userId as we declared that it has a default value earlier


-- uUserRow :: Write Hask TestDB User
-- Write Hask stands for Haskell writable
uUserRow = User Nothing (T.pack "brian") (Age 21) Male (AddressId 1) :: Write Hask TestDB User

-- userInsert :: ReaderT (Config a) PG ()
userInsert = insert (Tab @TestDB @User) uUserRow :: ReaderT (Config a) PG ()

-- userInsert' :: ReaderT (Config a) PG [User Hask]
userInsert' = insertRet (Tab @TestDB @User) uUserRow id :: ReaderT (Config a) PG [User Hask]

-- userInsert'' :: ReaderT (Config a) PG [User (Prj Hask '["name"])]
userInsert'' = insertRet (Tab @TestDB @User) uUserRow (project @'["name"]) :: ReaderT (Config a) PG [User _]

-- userDelete :: ReaderT (Config a) PG Int64
userDelete = delete (Tab @TestDB @User) (\_ -> constant False) :: ReaderT (Config a) PG Int64

-- userUpdate :: ReaderT (Config a) PG Int64
userUpdate = update (Tab @TestDB @User) (\_ -> constant uUserRow) (\_ -> constant False) :: ReaderT (Config a) PG Int64

-- Update and return the entire user 
-- userUpdate' :: ReaderT (Config a) PG [User Hask]
userUpdate' = updateRet (Tab @TestDB @User) (\_ -> constant uUserRow) (\_ -> constant False) id :: ReaderT (Config a) PG [User Hask]

-- Update and project user's age
-- userUpdate'' :: ReaderT (Config a) PG [User (Prj Hask '["age"])]
userUpdate'' = updateRet (Tab @TestDB @User) (\_ -> constant uUserRow) (\_ -> constant False) (project @'["age"]) :: ReaderT (Config a) PG [User _]

-- *Main> :t (rightTab . head) <$> getAll userLJoinQ


main :: IO ()
main = putStrLn "Test suite not yet implemented"
