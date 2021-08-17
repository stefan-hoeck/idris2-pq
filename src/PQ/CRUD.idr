module PQ.CRUD

import Control.Monad.Either
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
0 GetTypes : List Column -> List Type
GetTypes = map GetTypeC

public export
0 GetRow : List Column -> Type
GetRow cs = NP I (GetTypes cs)

colVals : (cs : List Column) -> PutRow cs -> List String
colVals [] [] = []
colVals (MkField _ n pqTpe _ dflt _ toPQ :: cs) (v :: vs) =
  encodePutType dflt (toPQ v) :: colVals cs vs

colStr : (cs : List Column) -> PutRow cs -> String
colStr cs row = 
  let vals = fastConcat $ intersperse ", " (colVals cs row)
   in #"(\#{vals})"#

export
insertSQL : (t : Table) -> List (PutRow (columns t)) -> String
insertSQL (MkTable n cs) rows =
  let colNames = fastConcat $ intersperse ", " (map name cs)
      allVals  = fastConcat $ intersperse ", " (map (colStr cs) rows)
   in #"INSERT INTO \#{n} (\#{colNames}) VALUES \#{allVals};"#

export
getSQL :  (t        : Table)
       -> (cs       : List Column)
       -> {auto 0 _ : Elems cs (columns t)}
       -> (query    : Op)
       -> String
getSQL t cs query =
  let cols = fastConcat $ intersperse ", " $ map name cs
   in #"SELECT \#{cols} FROM \#{t.name} WHERE \#{opToSQL query};"#

export
count : (t : Table) -> (query : Op) -> String
count t query = #"SELECT count(*) FROM \#{t.name} WHERE \#{opToSQL query};"#

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

export
insert :  HasIO io
       => MonadError SQLError io
       => Connection
       -> (t : Table)
       -> List (PutRow (columns t))
       -> io ()
insert c t rows = exec_ c (insertSQL t rows) COMMAND_OK

export
insert1 :  HasIO io
        => MonadError SQLError io
        => Connection
        -> (t : Table)
        -> PutRow (columns t)
        -> io ()
insert1 c t row = insert c t [row]

names : (cs : List Column) -> NP (K String) (GetTypes cs)
names []        = []
names (x :: xs) = x.name :: names xs

reader : (c : Column) -> String -> Maybe (GetTypeC c)
reader (MkField _ _ pqType _ _ fromPQ _) s =
  fromPQ (decodeDBType pqType s)

readers : (cs : List Column) -> NP (\t => String -> Maybe t) (GetTypes cs)
readers []        = []
readers (x :: xs) = reader x :: readers xs

export
get :  HasIO io
    => MonadError SQLError io
    => Connection
    -> (t        : Table)
    -> (cs       : List Column)
    -> {auto 0 _ : Elems cs (columns t)}
    -> (query : Op)
    -> io (List $ GetRow cs)
get c t cs query = do
  res <- exec c (getSQL t cs query) TUPLES_OK
  getRows (names cs) (readers cs) res

export
countCmd :  HasIO io
         => MonadError SQLError io
         => Connection
         -> (t     : Table)
         -> (query : Op)
         -> io Bits32
countCmd c t query = do
  res   <- exec c (count t query) TUPLES_OK
  [[c]] <- getRows ["count"] [Just . cast {to = Bits32}] res
    | _ => pure 0
  pure c

export
delete :  HasIO io
       => MonadError SQLError io
       => Connection
       -> (t     : Table)
       -> (query : Op)
       -> io ()
delete c t query =
  let sql = #"DELETE FROM \#{t.name} WHERE \#{opToSQL query};"#
   in exec_ c sql COMMAND_OK

export
clearTable :  HasIO io
           => MonadError SQLError io
           => Connection
           -> (t     : Table)
           -> io ()
clearTable c t = delete c t True

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
getIdName = getSQL MyTable [Name,Id] (Orders >= 3)
