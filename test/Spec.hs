{-# LANGUAGE DataKinds, Arrows, TypeApplications, PartialTypeSignatures, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, DeriveGeneric, GeneralizedNewtypeDeriving #-}

import Opaleye hiding (Column, Table, leftJoin, aggregate)
import qualified Opaleye as O
import Opaleye.Constant
import Opaleye.Record
import Database.Migration hiding (Column, def, Col)
import qualified Database.Migration as Mig
import Database.Transaction
import Data.Aeson
import Data.Profunctor.Product.Default
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Profunctor
import Control.Arrow

data TestDB

newtype Age = Age {getAge :: Int}
  deriving (Generic, Num)

newtype Age1 = Age1 {getAge1 :: Age}
  deriving (Generic, Num)

data Sum1 = Con1 | Con2
  deriving (Generic, Show, Read)

data UID = UID {uid :: Text} deriving (Generic)
instance ToJSON UID
instance FromJSON UID

data User f = User
  { user_id :: Col f "user_id" Age
  , name    :: Col f "name" Text
  , age     :: Col f "age" Sum1
  } deriving (Generic)

instance ( Profunctor p
         , Applicative (p (User f))
         , Default p (Col f "user_id" Age) (Col g "user_id" Age)
         , Default p (Col f "name" Text) (Col g "name" Text)
         , Default p (Col f "age" Sum1) (Col g "age" Sum1)
         ) => Default p (User f) (User g) where
  def = User <$> lmap user_id def
             <*> lmap name def
             <*> lmap age def

  
instance Database TestDB where
  type Tables TestDB = '[User Hask]
  type Types TestDB  = '[Sum1]

instance Table TestDB (User Hask) where
  type HasDefault (User Hask) = '["user_id"]

testRecQ :: Query (HList Identity (GetTableFields (User Op)))
testRecQ = proc () -> do
  u <- queryRec (Tab @TestDB @User) -< ()
  returnA -< u

testQ :: Query (User Op)
testQ = proc () -> do
  u <- query (Tab @TestDB @User) -< ()
  restrict -< name u .== constant (T.pack "dfdf")
  restrict -< age u .== constant Con1
  returnA -< u

--testPrjQ :: Query (User (Prj Op '["name", "age"]))
testPrjQ = proc () -> do
  u <- query (Tab @TestDB @User) -< ()
  returnA -< project @'["age", "name"] u

testPrjQ1 = proc () -> do
  u <- testPrjQ -< ()
  returnA -< project @'["name"] u

testPG :: PG [User Hask]
testPG = getAll testQ

testPrj :: PG _
testPrj = getAll testPrjQ

testTrans :: ReaderT (Config a) IO [User Hask]
testTrans = runTransaction testPG

testPGIO :: ReaderT (Config a) PG [User Hask]
testPGIO = getAll testQ

testLJoinQ = leftJoin testQ testQ (const $ (constant True :: ReadSpec Bool))
testLJoinQ' = leftJoin testPrjQ testQ (const $ (constant True :: ReadSpec Bool))
testLJoinQ'' = leftJoin testQ testPrjQ (const $ (constant True :: ReadSpec Bool))

testLJPrjQ = proc () -> do
  ljres <- testLJoinQ' -< ()
  returnA -< leftTab ljres

testLJPrjQ' = proc () -> do
  u <- testPrjQ -< ()
  ljres <- testLJoinQ' -< ()
  returnA -< leftTab ljres  

testLJoin = getAll testLJoinQ :: ReaderT (Config a) PG _

testAggQ1 = aggregate @'[Sum "user_id", GroupBy "name"] (Tab @TestDB @User) testQ
testAggQ2 = aggregate @'[Sum "age", GroupBy "name"] (Tab @TestDB @User) testPrjQ
testAggQ3 = Opaleye.Record.aggregateOrdered @'[Sum "user_id", Sum "age", GroupBy "name"] (Tab @TestDB @User) (asc name) testQ
testAggQ4 = aggregate @'[GroupBy "name"] (Tab @TestDB @User) testPrjQ1
testAggQ5 = aggregate @'[GroupBy "name"] (Tab @TestDB @User) testLJPrjQ'

testAgg = getAll testAggQ1 :: ReaderT (Config a) PG _


uUserRow = User Nothing (T.pack "fd") Con1 :: HaskW TestDB User

testInsert = insert (Tab @TestDB @User) (User (Just 1) (T.pack "fd") Con1) :: ReaderT (Config a) PG ()

testInsert' = insertRet (Tab @TestDB @User) (User Nothing (T.pack "fd") Con2) id :: ReaderT (Config a) PG [User Hask]
testInsert'' = insertRet (Tab @TestDB @User) (User Nothing (T.pack "fd") Con2) (project @'["name"]) :: ReaderT (Config a) PG [User _]
testDelete = delete (Tab @TestDB @User) (\_ -> constant False) :: ReaderT (Config a) PG Int64
testUpdate = update (Tab @TestDB @User) (\_ -> constant uUserRow) (\_ -> constant False) :: ReaderT (Config a) PG Int64
testUpdate' = updateRet (Tab @TestDB @User) (\_ -> constant uUserRow) (\_ -> constant False) id :: ReaderT (Config a) PG [User Hask]

testUpdate'' = updateRet (Tab @TestDB @User) (\_ -> constant uUserRow) (\_ -> constant False) (project @'["age"]) :: ReaderT (Config a) PG [User _]


main :: IO ()
main = putStrLn "Test suite not yet implemented"
