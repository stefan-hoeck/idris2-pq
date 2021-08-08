module PQ.CRUD

import Data.SOP
import PQ.Schema

%default total

public export
Row : (f : Column -> Type) -> List Column -> Type
Row = NP

--------------------------------------------------------------------------------
--          Create
--------------------------------------------------------------------------------

public export
CreateEffect : Constraint -> Default t -> Type -> Type
CreateEffect _ (Value x)          = Maybe
CreateEffect _ SmallSerial        = K ()
CreateEffect _ Serial             = K ()
CreateEffect _ BigSerial          = K ()
CreateEffect (Vanilla Nullable) _ = Maybe
CreateEffect (Unique Nullable)  _ = Maybe
CreateEffect _ NoDefault          = I

public export
CreateType : Column -> Type
CreateType (MkField _ tpe con def) =
  CreateEffect con def (IdrisType tpe)

public export
CreateRow : List Column -> Type
CreateRow = Row CreateType

--------------------------------------------------------------------------------
--          Example
--------------------------------------------------------------------------------

MyTable : Table
MyTable = MkTable "customer"
            [ MkField "id" BigInt PrimaryKey BigSerial
            , MkField "name" Text (Vanilla NotNull) NoDefault
            , MkField "orders" PQInteger (Vanilla NotNull) (Value 0)
            ]

newCustomer : CreateRow (columns MyTable)
newCustomer = [(), "Gundi", Nothing]
