module Adapter.PostgreSQL.Auth where

import           ClassyPrelude
import           Data.Pool
import           Database.PostgreSQL.Simple.Migration
import           Database.PostgreSQL.Simple

type State = Pool Connection

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _                  -> return ()
 where
  cmds =
    [ MigrationInitialization
    , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
    ]
