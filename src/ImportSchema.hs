import LappDriver
import Database.HDBC
import System.Environment
import System.Console.GetOpt
import Data.Maybe
import Data.Char
import qualified Data.List as L
import Data.List.Split
import System.Exit
import qualified Data.ByteString.Char8 as B
import System.Directory
import System.IO
import Data.Time
import Paths_importschema (version)
import Data.Version (showVersion)
    
data ColumnDesc = ColumnDescrip { columnName :: String
                                , columnHaskellName :: String
                                , columnType :: SqlTypeId
                                , columnTypeString :: String
                                , columnSize :: Int
                                , columnDefault :: String
                                } deriving Show

{- | Map a SqlTypeId to a Haskell type
-}
mapSqlTypeToType SqlCharT = "B.ByteString"
mapSqlTypeToType SqlVarCharT = "B.ByteString"
mapSqlTypeToType SqlIntegerT = "Integer"
mapSqlTypeToType SqlBitT = "Integer"
--mapSqlTypeToType SqlTimestampT = "B.ByteString"
mapSqlTypeToType SqlTimestampT = "LocalTime"
mapSqlTypeToType SqlFloatT = "Double"
mapSqlTypeToType SqlRealT = "Double"
mapSqlTypeToType SqlTinyIntT = "Integer"
mapSqlTypeToType t = show t

mapSqlTypeToDefault SqlCharT = "B.pack \"\""
mapSqlTypeToDefault SqlVarCharT = "B.pack \"\""
mapSqlTypeToDefault SqlIntegerT = "0"
mapSqlTypeToDefault SqlBitT = "0"
mapSqlTypeToDefault SqlTimestampT = "LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)"
mapSqlTypeToDefault SqlFloatT = "0.0"
mapSqlTypeToDefault SqlRealT = "0.0"
mapSqlTypeToDefault SqlTinyIntT = "0"
mapSqlTypeToDefault t = ""

{- | Create a ColumnDescription, effectively translating a SqlColDesc
   into more usable form
-}
createColumn :: (String, SqlColDesc) -> ColumnDesc
createColumn (name, desc) =
    ColumnDescrip name
                  ((checkIdName.convertColumnName) name)
                  (colType desc) ((mapSqlTypeToType . colType) desc)
                  size
                  (mapSqlTypeToDefault $ colType desc)
    where size = case (colSize desc) of
           Nothing -> 0
           Just x  -> x

checkIdName :: String -> String
checkIdName n 
    | n == "id" = "id'"
    | otherwise = n

convertColumnName :: String -> String
convertColumnName = checkIdName.convertColumnName''
                    
convertColumnName'' :: String -> String
convertColumnName'' n =
    if firstChar == '_'
    then tail nx
    else nx
    where nx = convertColumnName' n False
          firstChar = head nx

convertColumnName' [] _ = []
convertColumnName' (x: xs) flag =
    if (isAsciiUpper x)
    then under ++ [lx] ++ convertColumnName' xs True
    else x : convertColumnName' xs False
        where lx = toLower x
              under = if flag || x == '_' then "" else "_"

printSchemaData :: Handle -> String -> String -> String -> [ColumnDesc] -> String -> String -> IO ()
printSchemaData outF tableName dataName _ schema dbType modName = do
  hPutStrLn outF $ "data " ++ dataName ++ " = " ++ dataName ++ " { "
  let cols = map (\s -> (columnHaskellName s) ++ " :: " ++ columnTypeString s) schema
  mapM_ (hPutStrLn outF) $ map (\x -> "    " ++ x) $ head cols : (map (\x -> "," ++ x) (tail cols))
  hPutStrLn outF "} deriving (Show,Eq)"

{-| Produce a tuple containing either the supplied table and data type
  name, split on the ':' char, or two table names (i.e. use the table
  name for both.  Also splits off the id column
-}
splitTD td = case
    map B.unpack tds 
    of
      (x:xs:xss:_) -> (x, xs, xss)
      (x:xs:_)     -> (x, xs, "id")
      (x:_)        -> (x, x, x)
      (_)          -> ("", "", "id")
    where
      tdp = B.pack td
      tds = B.split ':' tdp

--printInstances :: String -> String -> Str
printInstances outF tableName dataName idCol schema dbType modName = do
  mapM_ (hPutStrLn  outF) [ "\ninstance DBMapping " ++ dataName ++ " where "
                          , "    idColumn _   = idColumn'"
                          , "    tableName _  = tableName'"
                          , "    mappingFind  = find" ++ dataName
                          , "    getId n      = " ++ modName ++ "." ++ (convertColumnName idCol) ++ " n"
                          , "    saveExisting = saveExisting" ++ dataName
                          , "    saveNew      = saveNew" ++ dataName
                          , ""
                          ]
           
printTableDetails outF tableName dataName idCol schema dbType modName = do
  hPutStrLn outF $ "\n-- Defaults and column names for " ++ tableName
  hPutStrLn outF $ "columnNames" ++ dataName ++ " :: [String]"
  hPutStr outF $ "columnNames" ++ dataName ++ " = ["
  hPutStr outF $ concat $ L.intersperse "," $ map (\x -> "\"" ++ (columnName x) ++ "\"") schema
  hPutStrLn outF "]"
  --hPutStrLn outF $ "-- idCol:" ++ idCol
  --hPutStrLn outF $ "--" ++ $ show (not $ compareNoCase "USerID" idCol)
  hPutStrLn outF $ "\nupdateColumnNames" ++ dataName ++ " :: [String]"
  hPutStr outF $ "updateColumnNames" ++ dataName ++ " = ["
  hPutStr outF $ concat $ L.intersperse "," $ map (\x -> "\"" ++ x ++ "\"")
             (filter (\x -> not $ compareNoCase x idCol) (map columnName schema))
  hPutStrLn outF "]"
  hPutStrLn outF $ "\nidColumn' :: String\nidColumn' = \"" ++ idCol ++ "\""
  hPutStrLn outF $ "\ndefault" ++ dataName ++ " :: " ++ dataName ++ "\ndefault" ++ dataName ++ " = " ++ dataName ++ "{" ++
            (concat $ L.intersperse ", " $
                   map (\x -> modName ++ "." ++ ((columnHaskellName) x) ++ " = " ++ (columnDefault x)) schema) ++ "}"


printHelperFunctions outF tableName dataName idCol schema dbType modName = do
  hPutStrLn outF $ "find" ++ dataName ++ " connection id' = do"
  hPutStrLn outF $ "   row <- findRowIO connection tableName' idColumn' id'"
  hPutStrLn outF $ "   return $ populate" ++ dataName ++ " row"
  hPutStrLn outF ""
  hPutStrLn outF $ "populateUser Nothing    = Nothing"
  hPutStrLn outF $ "populateUser (Just row) = Just $ " ++ dataName
  hPutStrLn outF $ concat $  map (\c -> "  (fromSql $ getColValueFromRow \"" ++ (columnName c) ++ "\" row)\n") schema
  hPutStrLn outF ""

getLastIdQuery :: String -> String
getLastIdQuery dbType = case dbType of
                          "MSSQL" -> "SELECT @@ID"
                          "MYSQL" -> "SELECT LAST_INSERT_ID()"
                                     
printSavers outF tableName dataName idCol schema dbType modName = do
  hPutStrLn outF $ "saveExistingUser :: IConnection conn => conn -> " ++ dataName ++ " -> IO Integer"
  hPutStrLn outF $ "-- Savers \n\
\saveExisting" ++ dataName ++ " connection n = \n\
\    do\n\
\      let query = \"UPDATE \" ++ tableName' ++ \" SET \" ++ colSet ++ \" WHERE \" ++ idColumn' ++ \"=?\" \n\
\      let vals = (getUpdateColValues n) ++ [toSql $ getId n]\n\
\      run connection query vals\n\
\      where colSet = concat $ intersperse \",\" $ map (\\x -> x ++ \"=?\") updateColumnNames" ++ dataName ++ "\n"

  hPutStrLn outF $ "saveNewUser :: IConnection conn => conn -> " ++ dataName ++ " -> IO Integer"
  hPutStrLn outF $ "saveNew" ++ dataName ++ " connection n = do\n\
\  let query = \"INSERT \" ++ tableName' ++ \"(\" ++ colNameSet ++ \") VALUES (\" ++ colParamSet ++ \")\"\n\
\  _ <- run connection query (getUpdateColValues n)"
  hPutStrLn outF $ "  run connection \"" ++ (getLastIdQuery dbType) ++ "\" []\n\
\    where colNameSet = concat $ intersperse \",\" updateColumnNames" ++ dataName ++ "\n\
\          colParamSet = concat $ intersperse \",\" $ take (length updateColumnNames" ++ dataName ++ ") (repeat \"?\")\n"

  hPutStrLn outF $ "getColValues :: " ++ dataName ++ " -> [SqlValue]\ngetColValues n =\n\
\    [" ++ concat (L.intersperse "    ," (map colstr columnNames)) ++ "    ]\n"
  --hPutStrLn outF $ "-- " ++ idCol
  hPutStrLn outF $ "getUpdateColValues :: " ++ dataName ++ " -> [SqlValue]\ngetUpdateColValues n =\n\
\    [" ++ concat (L.intersperse "    ," (map colstr updcolumnNames)) ++ "    ]"
    where updcolumnNames = filter (/= convertColumnName idCol) $ map (convertColumnName.columnName) schema
          columnNames = map (convertColumnName.columnName) schema
          colstr cn = "toSql $ " ++ cn ++ " n\n"
                                                
handleTable dbh outF dbType modName (tableName,dataName, idCol)  = do
  --putStrLn tableName
  hPutStrLn outF $ "--------------------------------------------\n-- Schema for '" ++ tableName ++ "' as '" ++ dataName ++ "'"
  hPutStrLn outF $ "\ntableName' :: String\ntableName' = \"" ++ tableName ++ "\"\n"
  sqlSchema <- describeTable dbh tableName
  --hPutStrLn outF $ show sqlSchema
  let schema = map createColumn sqlSchema
  -- putStrLn $ "-- SCHEMA:" ++ (show schema)
  mapM_ (\f -> f outF tableName dataName idCol schema dbType modName)
            [ printSchemaData, printTableDetails
            , printInstances, printHelperFunctions
            , printSavers]

  hPutStrLn outF $ "\n-- End " ++ tableName ++ "\n--------------------------------------------\n"

main = do
  args <- getArgs
  allopts <- processOpts args
  let opts = fst allopts
  if hasVersion opts
  then do
    putStrLn $ "ImportSchema " ++ (showVersion version)
    exitWith ExitSuccess
  else
      do
        curDateTime <- show `fmap` getCurrentTime
        let connInfo = getConnInfo opts
        dbh <- getConnection connInfo
        outF <- case Main.getFile opts of
                 Just f  -> do
                   putStrLn f
                   fileExists <- doesFileExist f
                   if fileExists
                   then removeFile f
                   else return  ()
                   openFile f System.IO.WriteMode
                 Nothing -> return System.IO.stdout
        let dbType = getDBType opts
        let modName = getModule opts
        hPutStrLn outF $ "{-\n Generated on " ++ curDateTime ++ "\n"
        hPutStrLn outF $ "  Connection info:" ++ connInfo ++ "\n"
        hPutStrLn outF $ "  Database type  :" ++ dbType ++ "\n"
        hPutStrLn outF $ "-}\n"
        hPutStrLn outF $ "module " ++ modName ++ " where\n"
        hPutStrLn outF "import Database.HDBC"
        hPutStrLn outF "import Database.HDBC.ODBC"
        hPutStrLn outF "import Mapping"
        hPutStrLn outF "import Data.List"
        hPutStrLn outF "import Data.Time"
        hPutStrLn outF "import Control.Applicative"
        hPutStrLn outF "import qualified Data.ByteString.Char8 as B\n"
        let tables = case Main.getTables opts of
                       Just ts -> ts
                       Nothing -> "tblUsers:User:id"
        let tableDetails = map splitTD $ splitOn "," tables
        -- hPutStrLn $ show tableDetails
        --putStrLn "Processing tables..."
        mapM_ (handleTable dbh outF dbType modName) tableDetails 
        hClose outF

compareNoCase :: String -> String -> Bool
compareNoCase l r = map toLower l == map toLower r
                    
-- Command line options
data Flag = Verbose | Version | Connection String | Tables String
          | Output String | Module String
          | DBType String
            deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['v'] ["verbose"] (NoArg Verbose) "Verbose"
    , Option ['V'] ["version"] (NoArg Version) "Display version info"
    , Option ['c'] ["connection"] (ReqArg Connection "CONN") "Connection name"
    , Option ['o'] ["output"] (ReqArg Output "FILE") "Output filename"
    , Option ['t'] ["tables"] (ReqArg Tables "TABLES") "List of table names"
    , Option ['m'] ["module"] (ReqArg Module "MODULE") "Module name"
    , Option ['d'] ["dbty[e"] (ReqArg DBType "DBTYPE") "Underlying Database type [MSSQL|MYSQL]"
    ]

outp, conn, tables :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"
conn = Connection . fromMaybe "DSN=shop2010m; UID=Bubble; PWD=P34c3d0ff"
tables = Tables . fromMaybe "tblUsers"

processOpts :: [String] -> IO ([Flag], [String])
processOpts argv =
    case getOpt Permute options argv of
      (o,n,[])   -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ImportSchema [OPTION]"

fromTables (Tables x) = Just x
fromTables _ = Nothing

fromVersion Version = True
fromVersion _       = False

hasVersion fs = any fromVersion fs

{- Module command line option -}
fromModule (Module m) = Just m
fromModule _          = Nothing

hasModule :: [Flag] -> Bool
hasModule fs = any (isJust . fromModule) fs

getModule :: [Flag] -> String
getModule fs = fromMaybe "Mappings.Generated" $ listToMaybe $ mapMaybe fromModule fs

-- DBType
fromDBType (DBType d) = Just d
fromDBType _          = Nothing

getDBType :: [Flag] -> String
getDBType fs = fromMaybe "MYSQL" $ listToMaybe $ mapMaybe fromDBType fs
               
hasTables :: [Flag] -> Bool
hasTables fs = any (isJust . fromTables) fs
getTables :: [Flag] -> Maybe String
getTables fs = listToMaybe $ mapMaybe fromTables fs

fromFile (Output f) = Just f
fromFile _          = Nothing
hasFile :: [Flag] -> Bool
hasFile fs = any (isJust . fromFile) fs
getFile :: [Flag] -> Maybe String
getFile fs = listToMaybe $ mapMaybe fromFile fs

fromConnInfo (Connection c) = Just c
fromConnInfo _              = Nothing
                              
getConnInfo fs = fromMaybe "DSN=shop2010m; UID=Bubble; PWD=P34c3d0ff" $ listToMaybe $ mapMaybe fromConnInfo fs

