module PQ.SQL.Type

import public Data.List.Elem

public export
data SQLType_ : (canWrap : Bool) -> Type where
  VarChar  : Nat -> SQLType_ canWrap
  Text     : SQLType_ canWrap
  SmallInt : SQLType_ canWrap
  SQLInt   : SQLType_ canWrap
  BigInt   : SQLType_ canWrap
  Boolean  : SQLType_ canWrap
  Array    : {0 canWrap1 : _ } -> SQLType_ canWrap1 -> SQLType_ canWrap
  Nullable : SQLType_ True -> SQLType_ False

public export
SQLType : Type
SQLType = SQLType_ False

public export
0 Repr : SQLType_ canWrap -> Type
Repr (VarChar k)  = String
Repr Text         = String
Repr SmallInt     = Int16
Repr SQLInt       = Int32
Repr BigInt       = Int64
Repr Boolean      = Bool
Repr (Array x)    = List (Repr x)
Repr (Nullable x) = Maybe (Repr x)

public export
record Attribute where
  constructor MkAttribute
  name : String
  type : SQLType

infixl 4 <->

public export %inline
(<->) : String -> SQLType -> Attribute
(<->) = MkAttribute

public export
Row : Type
Row = List Attribute

public export
data IsFieldName : (name : String) -> (row : Row) -> Type where
  Here  : (type : SQLType) -> IsFieldName name (MkAttribute name type :: attrs)
  There : IsFieldName name attrs -> IsFieldName name (attr :: attrs)

0 FieldType : IsFieldName name row -> SQLType
FieldType (Here tpe)  = tpe
FieldType (There fld) = FieldType fld

infixr 5 `And`
infixr 4 `Or`

public export
data Expr : (row : Row) -> SQLType -> Type where
  Null : {0 any : SQLType_ True} -> Expr row (Nullable any)

  Lit : {t : SQLType} -> Repr t -> Expr row t

  Field :  (name : String)
        -> {auto 0 prf : IsFieldName name row}
        -> Expr row (FieldType prf)

  (==) : Expr s t -> Expr s t -> Expr s Boolean
  (/=) : Expr s t -> Expr s t -> Expr s Boolean
  (>)  : Expr s t -> Expr s t -> Expr s Boolean
  (>=) : Expr s t -> Expr s t -> Expr s Boolean
  (<)  : Expr s t -> Expr s t -> Expr s Boolean
  (<=) : Expr s t -> Expr s t -> Expr s Boolean
  And  : Expr s Boolean -> Expr s Boolean -> Expr s Boolean
  Or   : Expr s Boolean -> Expr s Boolean -> Expr s Boolean
  Not  : Expr s Boolean -> Expr s Boolean
  IsNull    : Expr s (Nullable t) -> Expr s Boolean
  IsNotNull : Expr s (Nullable t) -> Expr s Boolean

  PlusSI  : Expr s SmallInt -> Expr s SmallInt -> Expr s SmallInt
  PlusI   : Expr s SQLInt   -> Expr s SQLInt   -> Expr s SQLInt
  PlusBI  : Expr s BigInt   -> Expr s BigInt   -> Expr s BigInt
  TimesSI : Expr s SmallInt -> Expr s SmallInt -> Expr s SmallInt
  TimesI  : Expr s SQLInt   -> Expr s SQLInt   -> Expr s SQLInt
  TimesBI : Expr s BigInt   -> Expr s BigInt   -> Expr s BigInt
  DivSI   : Expr s SmallInt -> Expr s SmallInt -> Expr s SmallInt
  DivI    : Expr s SQLInt   -> Expr s SQLInt   -> Expr s SQLInt
  DivBI   : Expr s BigInt   -> Expr s BigInt   -> Expr s BigInt
  ModSI   : Expr s SmallInt -> Expr s SmallInt -> Expr s SmallInt
  ModI    : Expr s SQLInt   -> Expr s SQLInt   -> Expr s SQLInt
  ModBI   : Expr s BigInt   -> Expr s BigInt   -> Expr s BigInt

--------------------------------------------------------------------------------
--          Expr Interface Implementations
--------------------------------------------------------------------------------

public export
FromString (Expr row Text) where
  fromString = Lit

public export
FromString (Expr row $ Nullable Text) where
  fromString = Lit . Just

public export
Num (Expr row SmallInt) where
  fromInteger = Lit . fromInteger
  (+) = PlusSI
  (*) = TimesSI

public export
Num (Expr row SQLInt) where
  fromInteger = Lit . fromInteger
  (+) = PlusI
  (*) = TimesI

public export
Num (Expr row BigInt) where
  fromInteger = Lit . fromInteger
  (+) = PlusBI
  (*) = TimesBI

--------------------------------------------------------------------------------
--          Queries
--------------------------------------------------------------------------------

public export
record NamedExpr (row : Row) where
  constructor MkNamedExpr
  0 tpe : SQLType
  name  : String
  expr  : Expr row tpe

public export %inline
as : Expr row tpe -> String -> NamedExpr row
as expr name = MkNamedExpr tpe name expr

public export
0 RowType : List (NamedExpr row) -> Row
RowType Nil = Nil
RowType (MkNamedExpr tpe name _ :: t) = MkAttribute name tpe :: RowType t

public export
data RowExpr : (row : Row) -> Type where
  Table : (tableName : String) -> (row : Row) -> RowExpr row
  Select :  (from   : RowExpr row)
         -> (filter : Expr row Boolean)
         -> (select : List (NamedExpr row))
         -> RowExpr (RowType select)

--------------------------------------------------------------------------------
--          Syntax and Tests
--------------------------------------------------------------------------------

Customer : Row
Customer = [ MkAttribute "customer.id" BigInt
           , MkAttribute "customer.name" Text
           , MkAttribute "customer.age" SmallInt
           , MkAttribute "customer.orders" SmallInt
           , MkAttribute "customer.city" Text
           ]

Customers : RowExpr Customer
Customers = Table "customers" Customer

aStringLit : Expr Customer Text
aStringLit = "foo"

aFieldRef : Expr Customer SmallInt
aFieldRef = Field "customer.age"

compExpr : Expr Customer Boolean
compExpr = Field "customer.name" == "foo"                     `And`
           Field "customer.id" >= 12                          `And`
           Field "customer.age" * 2 < Field "customer.orders"

topCustomers : RowExpr [ "name" <-> Text, "city" <-> Text, "age" <-> SmallInt ]
topCustomers = Select {
                 from   = Customers
               , filter = Field "customer.orders" >= 100 `And`
                          Field "customer.age" < 18
               , select = [ Field "customer.name" `as` "name"
                          , Field "customer.city" `as` "city"
                          , Field "customer.age"  `as` "age"
                          ]
               }
