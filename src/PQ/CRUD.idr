module PQ.CRUD

import Data.List
import Data.List.Elem
import Data.SOP
import PQ.FFI
import PQ.Schema
import PQ.Types

%default total

public export
data Elems : List a -> List a -> Type where
  Single : Elem x vs -> Elems [x] vs
  (::)   : Elem x vs -> Elems xs vs -> Elems (x :: xs) vs

public export
Row : (f : Column -> Type) -> List Column -> Type
Row = NP

--------------------------------------------------------------------------------
--          Insert
--------------------------------------------------------------------------------

public export
0 PutRow : List Column -> Type
PutRow = Row PutTypeC

public export
0 GetRow : List Column -> Type
GetRow = Row GetTypeC

colPairs : (cs : List Column) -> PutRow cs -> List (String, String)
colPairs [] [] = []
colPairs (MkField _ n pqTpe _ _ _ toPQ :: cs) (v :: vs) =
  case encodeDBType pqTpe <$> toPQ v of
    Just s  => (n, s) :: colPairs cs vs 
    Nothing => colPairs cs vs 

export
insert : (t : Table) -> PutRow (columns t) -> String
insert (MkTable n cs) row =
  let (cns,vs) = unzip $ colPairs cs row
      colNames = fastConcat $ intersperse ", " cns
      vals     = fastConcat $ intersperse ", " vs
   in #"INSERT INTO \#{n} (\#{colNames}) VALUES (\#{vals});"#

export
insertCmd : HasIO io => Connection -> (t : Table) -> PutRow (columns t) -> IO ()
insertCmd c t row = ignore $ exec c (insert t row)

export
get :  (t        : Table)
    -> (cs       : List Column)
    -> {auto 0 _ : Elems cs (columns t)}
    -> String
get t cs =
  let cols = fastConcat $ intersperse ", " $ map name cs
   in #"SELECT \#{cols} FROM \#{t.name};"#

export
getCmd :  HasIO io
       => (t        : Table)
       -> (cs       : List Column)
       -> {auto 0 _ : Elems cs (columns t)}
       -> io (Maybe $ List $ GetRow cs)

--------------------------------------------------------------------------------
--          Example
--------------------------------------------------------------------------------

Id : Column
Id = primarySerial64 Int64 "id" Just

Name : Column
Name =  notNull String "name" Text Just id

Orders : Column
Orders = notNullDefault Int32 "orders" PQInteger 0 Just id

MyTable : Table
MyTable = MkTable "customer" [Id, Name, Orders]

newCustomer : PutRow (columns MyTable)
newCustomer = [(), "Gundi", Nothing]

getIdName : String
getIdName = get MyTable [Name,Id]
