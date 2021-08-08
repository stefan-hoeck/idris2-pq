module PQ.Schema

import Data.List
import Data.String

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
IdrisType : PQType -> Type
IdrisType SmallInt        = Int16
IdrisType PQInteger       = Int32
IdrisType BigInt          = Int64
IdrisType DoublePrecision = Double
IdrisType Text            = String
IdrisType Boolean         = Bool

encodeIdrisType : (tpe : PQType) -> IdrisType tpe -> String
encodeIdrisType SmallInt x        = show x
encodeIdrisType PQInteger x       = show x
encodeIdrisType BigInt x          = show x
encodeIdrisType DoublePrecision x = show x
encodeIdrisType Text x            = #"'\#{x}'"#
encodeIdrisType Boolean x         = show x

--------------------------------------------------------------------------------
--          Table Columns
--------------------------------------------------------------------------------

public export
data Default : PQType -> Type where
  NoDefault   : {0 any : PQType} -> Default any
  Value       : {0 t   : PQType} -> IdrisType t -> Default t
  SmallSerial : Default SmallInt
  Serial      : Default PQInteger
  BigSerial   : Default BigInt

defaultSchema : {tpe : _} -> Default tpe -> String
defaultSchema NoDefault   = ""
defaultSchema (Value v)   = #" DEFAULT \#{encodeIdrisType tpe v}"#
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
record Column where
  constructor MkField
  name       : String
  tpe        : PQType
  constraint : Constraint
  deflt      : Default tpe

export
columnSchema : Column -> String
columnSchema (MkField name tpe con dflt) =
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
