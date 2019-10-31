module Adapter.PostgreSQL.Auth where

import           ClassyPrelude
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration

type State = Pool Connection

data Config = Config { cfgUrl :: ByteString
                     , cfgStripeCount :: Int
                     , cfgMaxOpenConnPerStripe :: Int
                     , cfgIdleConnTimeout :: NominalDiffTime
                     }

{- bracket

The 'bracket' function is comming from Control.Exception and is reexported by
ClassyPrelude. It encloses a ressource usage by defining a setup before and
some teardown operation after it has been used to free the resource.

Here we setup the connection pool for action and destruct the pool afterwards.

-}

-- | Sets up a data connection pool with user configuration
withPool :: Config -> (State -> IO a) -> IO a
withPool cfg action = bracket initPool cleanPool action
 where
  initPool = createPool openConn
                        closeConn
                        (cfgStripeCount cfg)
                        (cfgIdleConnTimeout cfg)
                        (cfgMaxOpenConnPerStripe cfg)
  cleanConn = destroyAllResources
  openConn  = connectPostgreSQL (cfgUrl cfg)
  closeConn = close

-- | Acquires connection from the pool through "withResource" and then runs the
-- migrations under src/Adapter/PostgreSQL/Migrations inalphabetical order
-- within a single transaction.
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

-- | Executes the database migration before executing the given action,
-- therefore the database is ensured to be called in its newest setup.
withState :: Config -> (State -> IO a) -> IO a
withState cfg action = withPool cfg $ \state -> do
  migrate state
  action state
