module Example.Main

import Control.Monad.Either
import Data.SOP
import Data.List.Elem
import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

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

main_ : HasIO io => MonadError SQLError io => io ()
main_ = do
  c    <- connect "postgresql://gundi@localhost:5432/testdb"
  rows <- getCmd c Tables [TableName,HasIndexes]
  finish c
  traverse_ printLn rows

main : IO ()
main = do Left err <- runEitherT (main_ {io = EitherT SQLError IO})
            | Right () => pure ()
          printLn err
