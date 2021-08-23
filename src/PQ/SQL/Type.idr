module PQ.SQL.Type

public export
data SQLType : (isNullable : Bool) -> Type where
  VarChar  : Nat -> SQLType False
  Text     : SQLType False
  SmallInt : SQLType False
  SQLInt   : SQLType False
  BigInt   : SQLType False
  Boolean  : SQLType False
  Array    : {0 isNullable : _ } -> SQLType isNullable -> SQLType False
  Nullable : SQLType False -> SQLType True

public export
0 Repr : SQLType isNullable -> Type
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
  type : SQLType isNullable

public export
Schema : Type
Schema = List Attribute

public export
data IsField :  (name : String)
             -> (type : SQLType isNullable)
             -> (schema : Schema)
             -> Type where
  Here  : IsField name type (MkAttribute name type :: attrs)
  There : IsField name type attrs -> IsField name type (attr :: attrs)

infixr 5 `And`
infixr 4 `Or`

public export
data Expr : (schema : Schema) -> SQLType isNullable -> Type where
  Null : {0 any : SQLType False} -> Expr schema (Nullable any)

  Lit : {t : SQLType False} -> Repr t -> Expr schema t

  NullableLit : {t : SQLType False} -> Repr t -> Expr schema (Nullable t)

  Field :  (name : String)
        -> {auto 0 _ : IsField name tpe schema}
        -> Expr schema tpe

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

textField : (n : String) -> {auto 0 prf : IsField n Text s} -> Expr s Text
textField = Field

--------------------------------------------------------------------------------
--          Expr Interface Implementations
--------------------------------------------------------------------------------

public export
FromString (Expr schema Text) where
  fromString = Lit

public export
FromString (Expr schema $ Nullable Text) where
  fromString = NullableLit

public export
Num (Expr schema SmallInt) where
  fromInteger = Lit . fromInteger
  (+) = PlusSI
  (*) = TimesSI

public export
Num (Expr schema SQLInt) where
  fromInteger = Lit . fromInteger
  (+) = PlusI
  (*) = TimesI

public export
Num (Expr schema BigInt) where
  fromInteger = Lit . fromInteger
  (+) = PlusBI
  (*) = TimesBI

--------------------------------------------------------------------------------
--          Syntax and Tests
--------------------------------------------------------------------------------

Customer : Schema
Customer = [ MkAttribute "id" BigInt
           , MkAttribute "name" Text
           , MkAttribute "age" SmallInt
           , MkAttribute "orders" SmallInt
           , MkAttribute "city" Text
           ]

aStringLit : Expr Customer Text
aStringLit = "foo"

aFieldRef : Expr Customer SmallInt
aFieldRef = Field "age"

compExpr : Expr Customer Boolean
compExpr = textField "name" == "foo"       `And`
           Field {tpe = BigInt} "id" >= 12 `And`
           Field {tpe = SmallInt} "age" * 2 < Field "orders"
