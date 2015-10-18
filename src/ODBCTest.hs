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
        xs <- getTables connection
        putStr $ "tables "++(foldr jn "." xs)++"\n"
        return ()
           where jn a b = a++" "++b
