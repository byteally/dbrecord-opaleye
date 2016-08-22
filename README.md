# dbrecord-opaleye

*dbrecord-opaleye* is a haskell library that
* Lets you share models across different layers (DB, API, App layer) 
* Automatically takes care of changing the shape of your model depending on the operation. (Projection, aggregation, left join)
* Provides sugar on top for Opaleye. dbrecord-opaleye enables you to fall back to plain Opaleye wherever you wish. 

Consider an example where you represent your user by the data type `User f`

```haskell
data User f = User
  { user_id :: Col f "user_id" Int
  , name    :: Col f "name" Text
  , age     :: Col f "age" Age
  , gender  :: Col f "gender" Gender
 } deriving (Generic)
```

* When you create a query with dbrecord-opaleye selecting all columns you would get


`(User Op)`


Op (code for Opaleye) in the above code snippet indicates all the fields within `User` are db values.
For example Int is reperestend as PGInt4 in Opaleye. So in `User Op`, `user_id` 's type would be PGInt4  




* When you run the query using `getAll` you would get 


`(User Hask)`


Hask indicates that all the fields within `User` are normal haskell values and in this case `userId` 's type would be `Int`





* On projection of name, age you would get 


`User (Prj Op '["name", "age"])`


All those fields that are not projected would have the type Void of the field's type. We have not projected Gender and user_id,  hence their types would be Void Gender and Void user_id respectively.

Benefits
---------
Almost all the ORMs conveniently ignore projections or lie about the types by having it as nullable in all scenarios to accommodate projection. This gives the caller the wrong nullability information about the field. With dbrecord-opaleye you can project any subset of the table and still work with the type defined for that table. This greatly reduced verbosity since you dont have to maintain n different combinations of a type depending on the operations you perform with them and also the caller will always clear indication of the shape of the data. For example they can't access the voided out field and do any meaningful operation. Doing so would result in a type error.


Example
----------
```haskell
-- You can create newtype to encode all domain specific 
-- information and use it seamlessly as a DB type enhancing type safety

newtype Age = Age {getAge :: Int}
  deriving (Generic, Num)

newtype UserId = UserId {getUserId :: Int}
  deriving (Generic, Num)

newtype AddressId = AddressId {getAddressId :: Int}
  deriving (Generic, Num)

data Gender = Male | Female | Other
  deriving (Generic, Show, Read)  

-- Our models
data User f = User
  { user_id    :: Col f "user_id" UserId
  , name       :: Col f "name" Text
  , age        :: Col f "age" Age
  , gender     :: Col f "gender" Gender
  , address_id :: Col f "address_id" AddressId
 } deriving (Generic)

data Address f = Address 
 { address_id  :: Col f "address_id" AddressId
 , street_1    :: Col f "street_1" Text
 , street_2    :: Col f "street_2" (Maybe Text)
 , city        :: Col f "city" Text
 , postal_code :: Col f "postal_code" Text

} deriving (Generic)


```


The schema information of the database can be defined using the following instances
which are part of [dbrecord](https://github.com/byteally/dbrecord). `dbrecord-opaleye` is built on top of [dbrecord](https://github.com/byteally/dbrecord)

```haskell
-- type for our DB
data TestDB

instance Database TestDB where
  type Tables TestDB = '[User Hask, Address Hask]
  type Types TestDB  = '[Age, Gender]

instance Table TestDB (User Hask) where
  type HasDefault (User Hask) = '["user_id"]

instance Table TestDB (Address Hask)

```


More query examples
------------------
```haskell


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

```

Moving to the Hask layer from DB layer using getAll
---------------------------------------------------

```haskell

-- getAll on UserQ would transform (User Op) to User Hask
userPG :: PG [User Hask]
userPG = getAll userQ

userPrj :: PG _ -- [User (Prj Hask '["age", "name"])]
userPrj = getAll userPrjQ

userTrans :: ReaderT (Config a) IO [User Hask]
userTrans = runTransaction userPG

userPGIO :: ReaderT (Config a) PG [User Hask]
userPGIO = getAll userQ
```



Left join
---------
```haskell

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

```



Aggregation
-----------

```haskell
-- userAggQ1 :: Query (User (Agg Op '['Sum "user_id", 'GroupBy "name"]))
userAggQ1 = aggregate @'[Sum "user_id", GroupBy "name"] (Tab @TestDB @User) userQ

-- userAggQ2 :: Query (User (Agg (Prj Op '["age", "name"]) '['Sum "age", 'GroupBy "name"]))
userAggQ2 = aggregate @'[Sum "age", GroupBy "name"] (Tab @TestDB @User) userPrjQ

-- userAggQ3 :: Query (User (Agg Op '['Sum "user_id", 'Sum "age", 'GroupBy "name"]))
userAggQ3 = Opaleye.Record.aggregateOrdered @'[Sum "user_id", Sum "age", GroupBy "name"] (Tab @TestDB @User) (asc name) userQ

-- userAggQ4 :: Query (User (Agg (Prj (Prj Op '["age", "name"]) '["name"]) '['GroupBy "name"]))
userAggQ4 = aggregate @'[GroupBy "name"] (Tab @TestDB @User) userPrjQ1

-- userAggQ5 :: Query (User (Agg (Prj Op '["age", "name"]) '['GroupBy "name"]))
userAggQ5 = aggregate @'[GroupBy "name"] (Tab @TestDB @User) userLJPrjQ'

-- userAgg :: ReaderT (Config a) PG [User (Agg Hask '['Sum "user_id", 'GroupBy "name"])]
userAgg = getAll userAggQ1 :: ReaderT (Config a) PG _

```


Insert, Update, Delete
--------------------------

```haskell
-- Note that we can send Nothing in place of userId as we declared that it has a default value earlier
-- uUserRow :: HaskW TestDB User
-- HaskW stands for Haskell writable
uUserRow = User Nothing (T.pack "brian") (Age 21) Male (AddressId 1) :: HaskW TestDB User

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

```


Pending items
-------------
* Fix aggregation on multiple relation composed into another type
* Migration feature for dbrecord


Would love to get your feedback and contributions are most welcome!


