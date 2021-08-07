module Example.Main

import PQ.FFI
import PQ.Types

main_ : HasIO io => io ()
main_ = do
  c    <- connect "postgresql://gundi@localhost:5432/testdb"
  s    <- status c
  res  <- exec c "SELECT current_time;"
  est  <- resultStatus res
  rows <- ntuples res
  cols <- nfields res
  val  <- getValue res 0 0
  
  putStrLn #"""
           Connection status : \#{show s}
           Result status     : \#{show est}
           Rows              : \#{show rows}
           Columns           : \#{show cols}
           Value             : \#{val}
           """#

  clear res
  finish c

main : IO ()
main = main_
