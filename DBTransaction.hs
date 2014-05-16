module DBTransaction where

import Control.Monad
import Control.Monad.Reader

import Database.HDBC
import Database.HDBC.PostgreSQL

databaseName = "test.db"
openOptions = "dbname=" ++ databaseName

openDatabase = connectPostgreSQL openOptions

type DBTransaction a = ReaderT Connection IO a
liftDB :: (a -> b) -> DBTransaction a -> DBTransaction b
liftDB = liftM

conn :: DBTransaction Connection
conn = ask

runTransaction :: DBTransaction a -> IO a
runTransaction act = do
  c <- openDatabase
  val <- (runReaderT act) c
  commit c
  err <- disconnect c
  return val

withConn :: (Connection -> IO a) -> DBTransaction a
withConn a = do
  c <- conn
  liftIO $ a c

runQuery :: String -> [SqlValue] -> DBTransaction ()
runQuery s v = withConn $ \c -> do 
  run c s v
  return ()

query :: String -> [SqlValue] -> DBTransaction [[SqlValue]]
query s v = withConn $ \c -> quickQuery' c s v

  
