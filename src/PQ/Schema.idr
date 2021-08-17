module PQ.Schema

import Control.Monad.Either

import Data.List
import Data.String
import Data.SOP

import PQ.FFI
import PQ.Types

%default total

--------------------------------------------------------------------------------
--          Data Types
--------------------------------------------------------------------------------

public export
data PQPrim : Type where
  SmallInt        : PQPrim
  PQInteger       : PQPrim
  BigInt          : PQPrim
  DoublePrecision : PQPrim
  Text            : PQPrim
  Boolean         : PQPrim

export
primSchema : PQPrim -> String
primSchema SmallInt        = "smallint"
primSchema PQInteger       = "integer"
primSchema BigInt          = "bigint"
primSchema DoublePrecision = "double precision"
primSchema Text            = "text"
primSchema Boolean         = "boolean"

public export
PrimType : PQPrim -> Type
PrimType SmallInt        = Int16
PrimType PQInteger       = Int32
PrimType BigInt          = Int64
PrimType DoublePrecision = Double
PrimType Text            = String
PrimType Boolean         = Bool

export
encodePrimType : (tpe : PQPrim) -> PrimType tpe -> String
encodePrimType SmallInt x        = show x
encodePrimType PQInteger x       = show x
encodePrimType BigInt x          = show x
encodePrimType DoublePrecision x = show x
encodePrimType Text x            = #"'\#{x}'"#
encodePrimType Boolean True      = "TRUE"
encodePrimType Boolean False     = "FALSE"

export
decodePrimType : (tpe : PQPrim) -> String -> PrimType tpe
decodePrimType SmallInt x        = cast x
decodePrimType PQInteger x       = cast x
decodePrimType BigInt x          = cast x
decodePrimType DoublePrecision x = cast x
decodePrimType Text x            = x
decodePrimType Boolean "t"       = True
decodePrimType Boolean _         = False

public export
data PQType : Type where
  NotNull  : PQPrim -> PQType
  Nullable : PQPrim -> PQType

public export
DBType : PQType -> Type
DBType (NotNull p)  = PrimType p
DBType (Nullable p) = Maybe $ PrimType p

export
encodeDBType : (tpe : PQType) -> DBType tpe -> String
encodeDBType (NotNull p)  v        = encodePrimType p v
encodeDBType (Nullable p) (Just v) = encodePrimType p v
encodeDBType (Nullable p) Nothing  = "DEFAULT"

export
decodeDBType : (tpe : PQType) -> String -> DBType tpe
decodeDBType (NotNull p)  s  = decodePrimType p s
decodeDBType (Nullable p) "" = Nothing
decodeDBType (Nullable p) s  = Just $ decodePrimType p s

--------------------------------------------------------------------------------
--          Table Columns
--------------------------------------------------------------------------------

public export
data Default : PQType -> Type where
  Null        : {0 any : PQPrim} -> Default (Nullable any)
  NoDefault   : {0 any : PQPrim} -> Default (NotNull any)
  Value       : {0 any : PQPrim} -> PrimType any -> Default (NotNull any)
  SmallSerial : Default (NotNull SmallInt)
  Serial      : Default (NotNull PQInteger)
  BigSerial   : Default (NotNull BigInt)

defaultSchema : {tpe : _} -> Default tpe -> String
defaultSchema SmallSerial = "SMALLSERIAL"
defaultSchema Serial      = "SERIAL"
defaultSchema BigSerial   = "BIGSERIAL"
defaultSchema {tpe = Nullable p} Null      = primSchema p
defaultSchema {tpe = NotNull p}  NoDefault = #"\#{primSchema p} NOT NULL"#
defaultSchema {tpe = NotNull p} (Value v)  =
  #"\#{primSchema p} DEFAULT \#{encodePrimType p v}"#

public export
data Constraint : Type where
  Vanilla      : Constraint
  PrimaryKey   : Constraint
  Unique       : Constraint

constraintSchema : Constraint -> String
constraintSchema Vanilla    = ""
constraintSchema PrimaryKey = " PRIMARY KEY"
constraintSchema Unique     = " UNIQUE"

public export
PutType : Default t -> Type -> Type
PutType Null        t = t
PutType NoDefault   t = t
PutType (Value x)   t = Maybe t
PutType SmallSerial _ = ()
PutType Serial      _ = ()
PutType BigSerial   _ = ()

public export
record Column where
  constructor MkField
  0 idrisTpe : Type
  name       : String
  pqType     : PQType
  constraint : Constraint
  deflt      : Default pqType
  fromPQ     : DBType pqType -> Maybe idrisType
  toPQ       : PutType deflt idrisTpe -> PutType deflt (DBType pqType)

export
encodePutType :  {tpe : _}
              -> (deflt : Default tpe)
              -> PutType deflt (DBType tpe)
              -> String
encodePutType Null x             = encodeDBType tpe x
encodePutType NoDefault x        = encodeDBType tpe x
encodePutType (Value y) (Just x) = encodeDBType tpe x
encodePutType (Value y) Nothing  = "DEFAULT"
encodePutType SmallSerial x      = "DEFAULT"
encodePutType Serial x           = "DEFAULT"
encodePutType BigSerial x        = "DEFAULT"

public export
0 PutTypeC : Column -> Type
PutTypeC (MkField idrisTpe _ _ _ def _ _) = PutType def idrisTpe

public export
0 GetTypeC : Column -> Type
GetTypeC c = c.idrisType

public export
primarySerial16 : (0 t : Type) -> String -> (Int16 -> Maybe t) -> Column
primarySerial16 t n f =
  MkField t n (NotNull SmallInt) PrimaryKey SmallSerial f id

public export
primarySerial32 : (0 t : Type) -> String -> (Int32 -> Maybe t) -> Column
primarySerial32 t n f =
  MkField t n (NotNull PQInteger) PrimaryKey Serial f id

public export
primarySerial64 : (0 t : Type) -> String -> (Int64 -> Maybe t) -> Column
primarySerial64 t n f =
  MkField t n (NotNull BigInt) PrimaryKey BigSerial f id

public export
notNull : (0 t : Type)
        -> String
        -> (pq : PQPrim)
        -> (decode : PrimType pq -> Maybe t)
        -> (encode : t -> PrimType pq)
        -> Column
notNull t n pq dec enc =
  MkField t n (NotNull pq) Vanilla NoDefault dec enc

public export
notNullDefault : (0 t : Type)
               -> String
               -> (pq   : PQPrim)
               -> (dflt : PrimType pq)
               -> (decode : PrimType pq -> Maybe t)
               -> (encode : t -> PrimType pq)
               -> Column
notNullDefault t n pq dflt dec enc =
  MkField t n (NotNull pq) Vanilla (Value dflt) dec (map enc)

public export
nullable : (0 t : Type)
         -> String
         -> (pq : PQPrim)
         -> (decode : PrimType pq -> Maybe t)
         -> (encode : t -> PrimType pq)
         -> Column
nullable t n pq dec enc =
  MkField (Maybe t) n (Nullable pq) Vanilla Null (maybe (Just Nothing) (map Just . dec)) (map enc)

export
columnSchema : Column -> String
columnSchema (MkField _ name tpe con dflt _ _) =
  #"\#{name} \#{defaultSchema dflt}\#{constraintSchema con}"#

--------------------------------------------------------------------------------
--          Table
--------------------------------------------------------------------------------

public export
record Table where
  constructor MkTable
  name    : String
  columns : List Column

export
createTableSQL : Table -> String
createTableSQL (MkTable name columns) =
  let cols = fastConcat $ intersperse ",\n" $ map (("  " ++) . columnSchema) columns
   in #"""
      CREATE TABLE \#{name} (
      \#{cols}
      );
      """#

export
createTable : HasIO io => MonadError SQLError io => Connection -> Table -> io ()
createTable c t = exec_ c (createTableSQL t) COMMAND_OK

--------------------------------------------------------------------------------
--          Queries
--------------------------------------------------------------------------------

public export
data EqPrim : PQPrim -> Type where
  EqSmallInt        : EqPrim SmallInt
  EqPQInteger       : EqPrim PQInteger
  EqBigInt          : EqPrim BigInt
  EqDoublePrecision : EqPrim DoublePrecision
  EqText            : EqPrim Text
  EqBoolean         : EqPrim Boolean

public export
data Eq : PQType -> Type where
  EqNotNull  : EqPrim p -> Eq (NotNull p)
  EqNullable : EqPrim p -> Eq (Nullable p)

public export
data OrdPrim : PQPrim -> Type where
  OrdSmallInt        : OrdPrim SmallInt
  OrdPQInteger       : OrdPrim PQInteger
  OrdBigInt          : OrdPrim BigInt
  OrdDoublePrecision : OrdPrim DoublePrecision
  OrdText            : OrdPrim Text
  OrdBoolean         : OrdPrim Boolean

public export
data Ord : PQType -> Type where
  OrdNotNull  : OrdPrim p -> Ord (NotNull p)
  OrdNullable : OrdPrim p -> Ord (Nullable p)

public export
data Op : Type where
  (==)  : (c : Column) -> DBType (pqType c) -> {auto 0 _ : Eq (pqType c)} -> Op
  (/=)  : (c : Column) -> DBType (pqType c) -> {auto 0 _ : Eq (pqType c)} -> Op
  (>)   : (c : Column) -> DBType (pqType c) -> {auto 0 _ : Ord (pqType c)} -> Op
  (>=)  : (c : Column) -> DBType (pqType c) -> {auto 0 _ : Ord (pqType c)} -> Op
  (<)   : (c : Column) -> DBType (pqType c) -> {auto 0 _ : Ord (pqType c)} -> Op
  (<=)  : (c : Column) -> DBType (pqType c) -> {auto 0 _ : Ord (pqType c)} -> Op
  (&&)  : Op -> Op -> Op
  (||)  : Op -> Op -> Op
  Not   : Op -> Op
  True  : Op
  False : Op

export
opToSQL : Op -> String
opToSQL (c == x) = #"\#{c.name} = \#{encodeDBType c.pqType x}"#
opToSQL (c /= x) = #"\#{c.name} <> \#{encodeDBType c.pqType x}"#
opToSQL (c > x)  = #"\#{c.name} > \#{encodeDBType c.pqType x}"#
opToSQL (c >= x) = #"\#{c.name} >= \#{encodeDBType c.pqType x}"#
opToSQL (c < x)  = #"\#{c.name} < \#{encodeDBType c.pqType x}"#
opToSQL (c <= x) = #"\#{c.name} <= \#{encodeDBType c.pqType x}"#
opToSQL (x && y) = #"(\#{opToSQL x}) AND (\#{opToSQL y})"#
opToSQL (Not x)  = #"NOT (\#{opToSQL x})"#
opToSQL (x || y) = #"(\#{opToSQL x}) OR (\#{opToSQL y})"#
opToSQL True     = "TRUE"
opToSQL False    = "FALSE"
