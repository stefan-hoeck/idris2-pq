module PQ.Types

import Generics.Derive

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          ConnStatusType
--------------------------------------------------------------------------------

||| `ConnStatusType` from `libpq-fe.h` but without the
||| `CONNECTION_` prefix.
public export
data ConnStatusType =
    OK
  | BAD
  | STARTED
  | MADE
  | AWAITING_RESPONSE
  | AUTH_OK
  | SETENV
  | SSL_STARTUP
  | NEEDED
  | CHECK_WRITABLE
  | CONSUME
  | GSS_STARTUP
  | CHECK_TARGET
  | CONN_STATUS_OTHER Bits8

%runElab derive "ConnStatusType" [Generic,Meta,Show,Eq,Ord]

namespace ConnStatusType
  public export
  fromBits8 : Bits8 -> ConnStatusType
  fromBits8  0 = OK
  fromBits8  1 = BAD
  fromBits8  2 = STARTED
  fromBits8  3 = MADE
  fromBits8  4 = AWAITING_RESPONSE
  fromBits8  5 = AUTH_OK
  fromBits8  6 = SETENV
  fromBits8  7 = SSL_STARTUP
  fromBits8  8 = NEEDED
  fromBits8  9 = CHECK_WRITABLE
  fromBits8 10 = CONSUME
  fromBits8 11 = GSS_STARTUP
  fromBits8 12 = CHECK_TARGET
  fromBits8  n = CONN_STATUS_OTHER n

--------------------------------------------------------------------------------
--          ExecStatusType
--------------------------------------------------------------------------------

public export
data ExecStatusType =
    EMPTY_QUERY
  | COMMAND_OK
  | TUPLES_OK
  | COPY_OUT
  | COPY_IN
  | BAD_RESPONSE
  | NONFATAL_ERROR
  | FATAL_ERROR
  | COPY_BOTH
  | SINGLE_TUPLE
  | EXEC_STATUS_OTHER Bits8

%runElab derive "ExecStatusType" [Generic,Meta,Show,Eq,Ord]

namespace ExecStatusType
  public export
  fromBits8 : Bits8 -> ExecStatusType
  fromBits8 0 = EMPTY_QUERY
  fromBits8 1 = COMMAND_OK
  fromBits8 2 = TUPLES_OK
  fromBits8 3 = COPY_OUT
  fromBits8 4 = COPY_IN
  fromBits8 5 = BAD_RESPONSE
  fromBits8 6 = NONFATAL_ERROR
  fromBits8 7 = FATAL_ERROR
  fromBits8 8 = COPY_BOTH
  fromBits8 9 = SINGLE_TUPLE
  fromBits8 n = EXEC_STATUS_OTHER n

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

public export
data SQLError : Type where
  ConnectionError : (type : ConnStatusType) -> (msg : String) -> SQLError

  ExecError       : (type : ExecStatusType) -> (msg : String) -> SQLError

  QueryError      :  (expectedNumerOfColumns : Bits32)
                  -> (numberOfColumns : Bits32)
                  -> SQLError

  ReadError       :  (name   : String)
                  -> (row    : Bits32)
                  -> (column : Bits32)
                  -> (value  : String)
                  -> SQLError

%runElab derive "SQLError" [Generic,Meta,Show,Eq]
