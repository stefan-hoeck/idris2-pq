module PQ.CRUD

import Data.SOP
import PQ.Schema
import Data.List

%default total

public export
Row : (f : Column -> Type) -> List Column -> Type
Row = NP

--------------------------------------------------------------------------------
--          Insert
--------------------------------------------------------------------------------

public export
0 InsertRow : List Column -> Type
InsertRow = Row PutTypeC

colPairs : (cs : List Column) -> InsertRow cs -> List (String, String)
colPairs [] [] = []
colPairs (MkField _ n pqTpe _ _ _ toPQ :: cs) (v :: vs) =
  case encodeDBType pqTpe <$> toPQ v of
    Just s  => (n, s) :: colPairs cs vs 
    Nothing => colPairs cs vs 

export
insert : (t : Table) -> InsertRow (columns t) -> String
insert (MkTable n cs) row =
  let (cns,vs) = unzip $ colPairs cs row
      colNames = fastConcat $ intersperse ", " cns
      vals     = fastConcat $ intersperse ", " vs
   in #"INSERT INTO \#{n} (\#{colNames}) VALUES (\#{vals})"#

--------------------------------------------------------------------------------
--          Example
--------------------------------------------------------------------------------

-- MyTable : Table
-- MyTable = MkTable "customer"
--             [ MkField "id" BigInt PrimaryKey BigSerial Int64 Just id
--             , MkField "name" Text (Vanilla NotNull) NoDefault String Just id
--             , MkField "orders" PQInteger (Vanilla NotNull) (Value 0) Int32 Just id
--             ]
-- 
-- newCustomer : InsertRow (columns MyTable)
-- newCustomer = [(), "Gundi", Nothing]
