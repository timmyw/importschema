import Database.HDBC
import Database.HDBC.ODBC
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
  then
      do
        putStrLn "Usage: ODBCTest CONNSTRING"
        exitWith ExitSuccess
  else
      do
        let connString = args !! 0
        putStrLn $ "ODBCTest: " ++ connString
        connection <- connectODBC connString
        let query = "SELECT count(*) FROM tblUsers"
        stmt <- prepare connection query
        rows <- fetchRow stmt
        return ()
    
