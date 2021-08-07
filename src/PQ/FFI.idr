module PQ.FFI

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
  | OTHER Bits8

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
  fromBits8  n = OTHER n
