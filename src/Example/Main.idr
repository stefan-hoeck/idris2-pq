module Example.Main

import Control.Monad.Either
import Data.SOP
import Data.List.Elem
import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

--------------------------------------------------------------------------------
--          Tables
--------------------------------------------------------------------------------

SchemaName : Column
SchemaName = notNull String "schemaname" Text Just id

TableName : Column
TableName = notNull String "tablename" Text Just id

TableOwner : Column
TableOwner = notNull String "tableowner" Text Just id

TableSpace : Column
TableSpace = nullable String "tablespace" Text Just id

HasIndexes : Column
HasIndexes = notNull Bool "hasindexes" Boolean Just id

HasRules : Column
HasRules = notNull Bool "hasrules" Boolean Just id

HasTriggers : Column
HasTriggers = notNull Bool "hastriggers" Boolean Just id

RowSecurity : Column
RowSecurity = notNull Bool "rowsecurity" Boolean Just id

Tables : Table
Tables = MkTable "pg_tables"
           [ SchemaName
           , TableName
           , TableOwner
           , TableSpace
           , HasIndexes
           , HasRules
           , HasTriggers
           , RowSecurity
           ]

tableExists :  HasIO io
            => MonadError SQLError io
            => Connection
            -> Table
            -> io Bool
tableExists c t =
  (1 ==) <$> countCmd c Tables (TableName == t.name)

--------------------------------------------------------------------------------
--          User
--------------------------------------------------------------------------------

Id : Column
Id = primarySerial64 Bits32 "user_id" (Just . cast)

Name : Column
Name = notNull String "user_name" Text Just id

Password : Column
Password = notNull String "user_password" Text Just id

Salary : Column
Salary = notNull Bits32 "user_salary" BigInt (Just . cast) cast

Age : Column
Age = notNull Bits8 "user_age" SmallInt (Just . cast) cast

Supervisor : Column
Supervisor = nullable Bits32 "user_supervisor" BigInt (Just . cast) cast

User : Table
User = MkTable "show_user"
         [Id, Name, Password, Salary, Age, Supervisor]

0 UserInsertRow : Type
UserInsertRow = PutRow (columns User)

users : List UserInsertRow
users = [ [(), "CEO",   "top secret", 15000,  58, Nothing]
        , [(), "Gundi", "123456",      4500,  45, Just 1]
        , [(), "Lars",  "Lars",        3500,  23, Just 2]
        , [(), "Eleni", "I love cats", 9000,  35, Just 1]
        ] ++ map johnDoe [2000 .. 40000]
  where johnDoe : Bits32 -> UserInsertRow
        johnDoe n = [(), "John Doe", "John Doe", n, 33, Just 3]

createAndFill :  HasIO io
              => MonadError SQLError io
              => Connection
              -> io ()
createAndFill c = do
  False <- tableExists c User
    | True => pure ()
  createTable c User
  insert c User users

--------------------------------------------------------------------------------
--          Main function
--------------------------------------------------------------------------------

main_ : HasIO io => MonadError SQLError io => io ()
main_ = do
  c    <- connect "postgresql://gundi@localhost:5432/testdb"
  createAndFill c
  rows <- get c User [Id,Name,Supervisor] (Name /= "Eleni" && Age >= 33)
  finish c
  traverse_ printLn rows

main : IO ()
main = do Left err <- runEitherT (main_ {io = EitherT SQLError IO})
            | Right () => pure ()
          printLn err
