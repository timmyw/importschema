{-# LANGUAGE FlexibleContexts #-}

module Mapping -- (DBMapping, mappingFind, findRowIO, getColValueFromRow)
    where

import Database.HDBC
import Database.HDBC.ODBC
import Data.Maybe
--import Data.Convertible.Base
import Database.HDBC.SqlValue
        
{-| This is a first pass attempt at creating a mapping that can
  load/modify/save records from tblUsers (or `users` while we test)
-}
    
-- | Group together all the ORM like functionality, find, save, new etc.
class DBMapping n where

     -- | Find and instantiate a record based on the supplied identity
     mappingFind :: Connection -> Integer -> IO (Maybe n)
         
     -- | Return the mapping's unique identity.  This shouldn't be a
     -- database lookup, but simply returning the identity value if it
     -- exists.
     getId :: n -> Integer

     -- | Populate the data record with the supplied database row
     --populate :: Maybe [SqlValue] -> n

     -- | The name of the under lying table
     tableName :: n -> String

     -- | The identity column name
     idColumn :: n -> String

     -- | Save a row
     save :: Connection -> n -> IO Integer
     save connection n = do
                    let id = getId n
                    existCnt <- doesIdExist connection (tableName n) (idColumn n) id
                    if existCnt > 0
                    then saveExisting connection n
                    else saveNew connection n

     saveExisting :: Connection -> n -> IO Integer

     saveNew :: Connection -> n -> IO Integer

     -- | Delete a row
     --delete :: Connection -> n -> IO Integer
               
-- | Try and find a row from the specified table based on the supplied
-- id.  Returns a Maybe [SqlValue]
findRowIO :: IConnection conn => conn -> String -> String -> Integer -> IO (Maybe [(String, SqlValue)])
findRowIO dbh tableName columnName id = do
  let query = "SELECT * FROM " ++ tableName ++ " WHERE " ++ columnName ++ " = " ++ (show id)
  putStrLn query
  stmt <- prepare dbh query
  execute stmt []
  res <- fetchRowAL stmt
  finish stmt
  return res

--findRowById :: IConnection conn => conn -> String -> String -> Integer -> IO Integer
doesIdExist dbh tableName columnName id = do
    let query = "SELECT COUNT(*) FROM " ++ tableName ++ " WHERE " ++ columnName ++ " = " ++ (show id)
    putStrLn query
    executeScalarInt dbh query 0
                
-- | Retrieve the named column value for a list of tuples
getColValueFromRow columnName [] = error $ columnName ++ " not found"
getColValueFromRow columnName ((name,val): nvs) = if columnName == name
                                                  then val
                                                  else getColValueFromRow columnName nvs

getValue v def =
    case v of
      Nothing -> def
      Just x  -> fromSql $ head x

-- | Execute a query that returns a single scalar and return that
executeScalarInt :: IConnection conn => conn -> String -> Integer -> IO Integer
--executeScalarInt :: (Convertible SqlValue a, IConnection b) => b -> String -> a -> IO a
executeScalarInt dbh query defValue = do
  stmt <- prepare dbh query
  execute stmt []
  rows <- fetchRow stmt
  let res = getValue rows defValue
  finish stmt
  return res
