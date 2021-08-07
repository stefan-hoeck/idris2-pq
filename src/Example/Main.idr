module Example.Main

import PQ.FFI
import PQ.Types

createWeather : String
createWeather = #"""
  CREATE TABLE weather (
    city            varchar(80),
    temp_lo         int,           -- low temperature
    temp_hi         int,           -- high temperature
    prcp            real,          -- precipitation
    date            date
  );
  """#

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

  res2 <- exec c createWeather
  est2 <- resultStatus res2
  msg  <- resultErrorMsg res2
  
  putStrLn #"""
           Result status     : \#{show est2}
           Error             : \#{msg}
           """#

  clear res2
  finish c

main : IO ()
main = main_
