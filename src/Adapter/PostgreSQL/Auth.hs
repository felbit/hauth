module Adapter.PostgreSQL.Auth where

import           ClassyPrelude
import           Data.Has
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import qualified Domain.Auth                   as D
import           Text.StringRandom

type State = Pool Connection

type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

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

-- | Helper function for PostgreSQL related functions. Gets the connection from
-- the pool and hands it over to the given action parameter.
withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool (\conn -> action conn)

{- try

"try" is importy by ClassyPrelude. It executes the first argument and catches
every synchronous exception, if any, with the type of the exception. If an
exception is thrown then the function will return `Left e`. If no exception
is thrown, try returns `Right a`.

-}

-- | Generates a new verification code (random 16 character string that has to
-- be unique systemwide - that's why this is concatenated with the unique
-- email string) similar to newSession'. Tries to add the auth to the database
-- and returns UserId and VerificationCode if successful. If an error occures
-- it tries to interpred the error and give the user a precise hint (e.g.
-- EmailTaken) or presents the error to the user if no precise reason can be
-- given.
addAuth
  :: PG r m
  => D.Auth
  -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.Auth email passw) = do
  let rawEmail = D.rawEmail email
      rawPassw = D.rawPassword passw
  -- generate vCode
  vCode <- liftIO $ do
    r <- stringRandomIO "[A-Za-z0-9]{16}"
    return $ (tshow rawEmail) <> "_" <> r
  -- issue query
  result <- withConn $ \conn -> try (query conn qry (rawEmail, rawPassw, vCode))
  {- interpreting result
  I am expecting one of these results:
    - the userId returned as success state
    - any other Right value means I really messed things up
    - any Left return coming from "try" is actually an exception caught by "try"
      so I will dig into that:
      - if the exception is about email_key it means that the user tried an
        email that is already taken. That might happen on a very regular basis.
      - any other exception has to be given to the user for further handling
  -}
  case result of
    Right [Only uId] -> return (Right (uId, vCode))
    Right _          -> throwString "Should not happen: PG doesn't return uId!"
    Left err@SqlError { sqlState = state, sqlErrorMsg = msg } ->
      if state == "23505" && "auths_email_key" `isInfixOf` msg
        then return $ Left D.RegistrationErrorEmailTaken
        else throwString ("Unhandled PG exception: " <> show err)
 where
  qry
    = "insert into auths \
        \(email, passwd, email_verification_code, is_email_verified) \
        \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"

-- | Sets the email address to a verified state if the verification code
-- matches.
setEmailAsVerified
  :: PG r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  result <- withConn $ \conn -> query conn qry (Only vCode)
  case result of
    [(uId, email)] -> case D.mkEmail email of
      Right email' -> return (Right (uId, email'))
      _ -> throwString $ "Should not happen: email in DB is not valid:
           " <> unpack email
    _ -> reutnr (Left D.EmailVerificationErrorInvalidCode)
 where
  qry
    = "update auths \
      \set is_email_verified = 't' \
      \where email_verification_code = ? \
      \returning id, cast (email as text)"
      -- the strange placement of '=' is done by brittany

-- | Get's the user by email and password and returns the user id (if any) with
-- the verficiation state
findUserByAuth :: PG r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth (D.Auth email password) = do
  let rawEmail    = D.rawEmail email
      rawPassword = D.rawPassword password
  result <- withConn (\conn -> query conn qry (rawEmail, rawPassword))
  return $ case result of
    [(uId, isVerified)] -> Just (uId, isVerified)
    _                   -> Nothing
 where
  qry = "select id, is_email_verified from auths \
        \where email = ? and pass = crypt(?, pass)"


-- | Gets an email address from the auths table by user id; returns Nothing if
-- id not found.
findEmailFromUserId :: PG r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  result <- withConn (\conn -> query conn qry (Only uId))
  case result of
    [Only email] -> case D.mkEmail email of
      Right email' -> return (Just email')
      _ -> throwString $ "Should not happen: email in DB is invalid:
      " <> unpack email
    _ -> return Nothing
 where
  qry = "select cast(email as text) from auths \
        \where id = ?"