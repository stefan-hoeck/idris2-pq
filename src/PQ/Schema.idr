module PQ.Schema

import Data.List
import Data.String
import Data.SOP

%default total

--------------------------------------------------------------------------------
--          Data Types
--------------------------------------------------------------------------------

public export
data PQType : Type where
  SmallInt        : PQType
  PQInteger       : PQType
  BigInt          : PQType
  DoublePrecision : PQType
  Text            : PQType
  Boolean         : PQType

export
typeSchema : PQType -> String
typeSchema SmallInt        = "smallint"
typeSchema PQInteger       = "integer"
typeSchema BigInt          = "bigint"
typeSchema DoublePrecision = "double precision"
typeSchema Text            = "text"
typeSchema Boolean         = "boolean"

public export
DBType : PQType -> Type
DBType SmallInt        = Int16
DBType PQInteger       = Int32
DBType BigInt          = Int64
DBType DoublePrecision = Double
DBType Text            = String
DBType Boolean         = Bool

export
encodeDBType : (tpe : PQType) -> DBType tpe -> String
encodeDBType SmallInt x        = show x
encodeDBType PQInteger x       = show x
encodeDBType BigInt x          = show x
encodeDBType DoublePrecision x = show x
encodeDBType Text x            = #"'\#{x}'"#
encodeDBType Boolean x         = show x

--------------------------------------------------------------------------------
--          Table Columns
--------------------------------------------------------------------------------

public export
data Default : PQType -> Type where
  NoDefault   : {0 any : PQType} -> Default any
  Value       : {0 t   : PQType} -> DBType t -> Default t
  SmallSerial : Default SmallInt
  Serial      : Default PQInteger
  BigSerial   : Default BigInt

defaultSchema : {tpe : _} -> Default tpe -> String
defaultSchema NoDefault   = ""
defaultSchema (Value v)   = #" DEFAULT \#{encodeDBType tpe v}"#
defaultSchema SmallSerial = " SERIAL"
defaultSchema Serial      = " SERIAL"
defaultSchema BigSerial   = " SERIAL"

public export
data Nullability = Nullable | NotNull

public export
data Constraint : Type where
  Vanilla      : Nullability -> Constraint
  PrimaryKey   : Constraint
  Unique       : Nullability -> Constraint

constraintSchema : Constraint -> String
constraintSchema (Vanilla Nullable) = ""
constraintSchema (Vanilla NotNull)  = " NOT NULL"
constraintSchema PrimaryKey         = " PRIMARY KEY"
constraintSchema (Unique Nullable)  = " UNIQUE"
constraintSchema (Unique NotNull)   = " UNIQUE NOT NULL"

public export
PutType : Constraint -> Default t -> Type -> Type
PutType _                  (Value x)   t = Maybe t
PutType _                  SmallSerial _ = ()
PutType _                  Serial      _ = ()
PutType _                  BigSerial   _ = ()
PutType (Vanilla Nullable) _           t = Maybe t
PutType (Unique Nullable)  _           t = Maybe t
PutType _                  NoDefault   t = t

public export
GetType : Constraint -> Default t -> Type -> Type
GetType _                  (Value _) t = t
GetType (Vanilla Nullable) _         t = Maybe t
GetType (Unique Nullable)  _         t = Maybe t
GetType _                  _         t = t

public export
record Column where
  constructor MkField
  0 idrisTpe : Type
  name       : String
  pqType     : PQType
  constraint : Constraint
  deflt      : Default pqType
  fromPQ     : DBType pqType -> Maybe idrisTpe 
  toPQ       : PutType constraint deflt idrisTpe -> Maybe (DBType pqType)

public export
0 PutTypeC : Column -> Type
PutTypeC (MkField idrisTpe _ _ con def _ _) = PutType con def idrisTpe

public export
0 GetTypeC : Column -> Type
GetTypeC (MkField idrisTpe _ _ con def _ _) = GetType con def idrisTpe

public export
primarySerial16 : (0 t : Type) -> String -> (Int16 -> Maybe t) -> Column
primarySerial16 t n f =
  MkField t n SmallInt PrimaryKey SmallSerial f (const Nothing)

public export
primarySerial32 : (0 t : Type) -> String -> (Int32 -> Maybe t) -> Column
primarySerial32 t n f =
  MkField t n PQInteger PrimaryKey Serial f (const Nothing)

public export
primarySerial64 : (0 t : Type) -> String -> (Int64 -> Maybe t) -> Column
primarySerial64 t n f =
  MkField t n BigInt PrimaryKey BigSerial f (const Nothing)

public export
notNull : (0 t : Type)
        -> String
        -> (pq : PQType)
        -> (decode : DBType pq -> Maybe t)
        -> (encode : t -> DBType pq)
        -> Column
notNull t n pq dec enc =
  MkField t n pq (Vanilla NotNull) NoDefault dec (Just . enc)

public export
notNullDefault : (0 t : Type)
               -> String
               -> (pq   : PQType)
               -> (dflt : DBType pq)
               -> (decode : DBType pq -> Maybe t)
               -> (encode : t -> DBType pq)
               -> Column
notNullDefault t n pq dflt dec enc =
  MkField t n pq (Vanilla NotNull) (Value dflt) dec (map enc)

public export
nullable : (0 t : Type)
         -> String
         -> (pq : PQType)
         -> (decode : DBType pq -> Maybe t)
         -> (encode : t -> DBType pq)
         -> Column
nullable t n pq dec enc =
  MkField t n pq (Vanilla Nullable) NoDefault dec (map enc)

export
columnSchema : Column -> String
columnSchema (MkField _ name tpe con dflt _ _) =
  #"\#{name} \#{typeSchema tpe}\#{constraintSchema con}\#{defaultSchema dflt}"#

--------------------------------------------------------------------------------
--          Table
--------------------------------------------------------------------------------

public export
record Table where
  constructor MkTable
  name    : String
  columns : List Column

export
createTable : Table -> String
createTable (MkTable name columns) =
  let cols = fastConcat $ intersperse "," $ map columnSchema columns
   in #"CREATE TABLE \#{name} (\#{cols});"#
