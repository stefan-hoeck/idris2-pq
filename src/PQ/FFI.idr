module PQ.FFI

import PQ.Types

--------------------------------------------------------------------------------
--          Foreign Types
--------------------------------------------------------------------------------

public export
data PGConnection : Type where

public export
data PGResult : Type where

public export
Connection : Type
Connection = Ptr PGConnection

public export
Result : Type
Result = Ptr PGResult

--------------------------------------------------------------------------------
--          FFI
--------------------------------------------------------------------------------

pq : String -> String
pq s = #"C:\#{s},libpq"#

%foreign pq "PQconnectdb"
prim__connect : String -> PrimIO Connection

%foreign pq "PQstatus"
prim__status : Connection -> PrimIO Bits8

%foreign pq "PQfinish"
prim__finish : Connection -> PrimIO ()

%foreign pq "PQexec"
prim__exec : Connection -> String -> PrimIO Result

%foreign pq "PQntuples"
prim__ntuples : Result -> PrimIO Bits32

%foreign pq "PQnfields"
prim__nfields : Result -> PrimIO Bits32

%foreign pq "PQresultStatus"
prim__resultStatus : Result -> PrimIO Bits8

%foreign pq "PQresultErrorMessage"
prim__resultErrorMsg : Result -> PrimIO String

%foreign pq "PQclear"
prim__clear : Result -> PrimIO ()

%foreign pq "PQgetvalue"
prim__getValue : Result -> Bits32 -> Bits32 -> PrimIO String

--------------------------------------------------------------------------------
--          Low level API
--------------------------------------------------------------------------------

export
connect : HasIO io => String -> io Connection
connect s = primIO (prim__connect s)

export
status : HasIO io => Connection -> io ConnStatusType
status c = fromBits8 <$> primIO (prim__status c)

export
finish : HasIO io => Connection -> io ()
finish c = primIO $ prim__finish c

export
exec : HasIO io => Connection -> String -> io Result
exec c s = primIO (prim__exec c s)

export
ntuples : HasIO io => Result -> io Bits32
ntuples r = primIO $ prim__ntuples r

export
nfields : HasIO io => Result -> io Bits32
nfields r = primIO $ prim__nfields r

export
resultStatus : HasIO io => Result -> io ExecStatusType
resultStatus r = fromBits8 <$> primIO (prim__resultStatus r)

export
resultErrorMsg : HasIO io => Result -> io String
resultErrorMsg r = primIO (prim__resultErrorMsg r)

export
clear : HasIO io => Result -> io ()
clear r = primIO $ prim__clear r

export
getValue : HasIO io => Result -> Bits32 -> Bits32 -> io String
getValue r row col = primIO $ prim__getValue r row col
