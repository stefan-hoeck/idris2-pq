module PQ.FFI.Bindings

import PQ.FFI.Types

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

%foreign pq "PQerrorMessage"
prim__errorMessage : Connection -> PrimIO String

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
--          WithConnection and WithRes
--------------------------------------------------------------------------------

public export
record SQLEnv (env : Type) (io : Type -> Type) (a : Type) where
  constructor MkSQLEnv
  runSQL : env -> io (Either SQLError a)

--------------------------------------------------------------------------------
--          Low level API
--------------------------------------------------------------------------------

-- public export
-- SQL : (Type -> Type) -> Type
-- SQL io = (HasIO io, MonadError SQLError io)
-- 
-- public export
-- WithConnection : (Type -> Type) -> Type -> Type
-- WithConnection io a =
--   HasIO io => MonadError SQLError io => Connection -> io a
-- 
-- public export
-- WithResult : (Type -> Type) -> Type -> Type
-- WithResult io a =
--   HasIO io => MonadError SQLError io => Result -> io a
-- 
-- export
-- status : WithConnection io ConnStatusType
-- status c = fromBits8 <$> primIO (prim__status c)
--  
-- export
-- connect : SQL io => String -> WithConnection io a -> io a
-- connect s f = do
--   c <- primIO (prim__connect s)
--   BAD <- status c
--     | _ => do
--       ea <- tryError (f c)
--       primIO $ prim__finish c
--       liftEither ea
-- 
--   msg <- primIO (prim__errorMessage c)
--   primIO $ prim__finish c
--   throwError $ ConnectionError BAD msg
-- 
-- export
-- resultStatus : WithResult io ExecStatusType
-- resultStatus r = fromBits8 <$> primIO (prim__resultStatus r)
-- 
-- export
-- resultErrorMsg : WithResult io String
-- resultErrorMsg r = primIO (prim__resultErrorMsg r)
-- 
-- export
-- exec :  String
--      -> ExecStatusType
--      -> WithResult io a
--      -> WithConnection io a
-- exec s est f c = do
--   res <- primIO (prim__exec c s)
--   st  <- resultStatus res
--   if st == est
--      then do
--        ea <- tryError (f res)
--        primIO $ prim__clear res
--        liftEither ea
--      else do
--        msg <- resultErrorMsg res
--        primIO $ prim__clear res
--        throwError $ ExecError st msg
-- 
-- pureRes : b -> WithResult io b
-- pureRes b _ = pure b

-- export
-- exec_ :  String -> ExecStatusType -> WithConnection io ()
-- exec_ s est = exec s est (pureRes {io} ())

--       => MonadError SQLError io
--       => Connection
--       -> String
--       -> ExecStatusType
--       -> io ()
-- exec_ c s est = exec c s est >>= clear
-- 
-- export
-- ntuples : HasIO io => Result -> io Bits32
-- ntuples r = primIO $ prim__ntuples r
-- 
-- export
-- nfields : HasIO io => Result -> io Bits32
-- nfields r = primIO $ prim__nfields r
-- 
-- export
-- getValue : HasIO io => Result -> Bits32 -> Bits32 -> io String
-- getValue r row col = primIO $ prim__getValue r row col

-- --------------------------------------------------------------------------------
-- --          SOP
-- --------------------------------------------------------------------------------
-- 
-- getVal :  HasIO io
--        => MonadError SQLError io
--        => Result
--        -> (row  : Bits32)
--        -> (name : String)
--        -> (col  : Bits32)
--        -> (read : String -> Maybe t)
--        -> io t
-- getVal res row name col read = do
--   s <- getValue res row col
--   case read s of
--     Just t => pure t
--     Nothing => throwError $ ReadError name row col s
-- 
-- getRow :  {0 ts : List Type}
--        -> HasIO io
--        => MonadError SQLError io
--        => NP (K String) ts
--        -> NP (K Bits32) ts
--        -> NP (\t => String -> Maybe t) ts
--        -> Result
--        -> (row : Bits32)
--        -> io (NP I ts)
-- getRow names columns readers res row =
--   sequenceNP $ hliftA3 (getVal res row) names columns readers
-- 
-- export
-- getRows :  {0 ts : List Type}
--         -> HasIO io
--         => MonadError SQLError io
--         => NP (K String) ts
--         -> NP (\t => String -> Maybe t) ts
--         -> Result
--         -> io (List $ NP I ts)
-- getRows names readers res =
--   let fieldCount = cast {to = Bits32} . length $ collapseNP names
--       indices    = the (NP (K Bits32) ts) $ iterateNP names (+1) 0
--    in do
--       nt <- ntuples res
--       if nt == 0
--          then pure []
--          else do
--            nf <- nfields res
--            if nf == fieldCount
--               then traverse (getRow names indices readers res) [0 .. nt - 1]
--               else throwError $ QueryError fieldCount nf
