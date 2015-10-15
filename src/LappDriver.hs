module LappDriver where

import Database.HDBC    
import Database.HDBC.ODBC

getConnection :: String -> IO Connection
getConnection connInfo = do
  putStrLn $ "Connecting to :" ++ connInfo
  connectODBC connInfo
                
