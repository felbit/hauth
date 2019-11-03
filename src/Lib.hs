module Lib where

import qualified Adapter.InMemory.Auth         as M
import qualified Adapter.PostgreSQL.Auth       as PG
import           ClassyPrelude
import qualified Control.Monad.Catch           as Catch
import           Domain.Auth
import           Katip

type State = (PG.State, TVar M.State)

newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO
             , KatipContext, Katip, Catch.MonadThrow)

run :: LogEnv -> State -> App a -> IO a
run le state = runKatipContextT le () mempty . flip runReaderT state . unApp

-- Glueing together the database and domain logic with instances of
-- AuthRepo, EmailVerificationNotif and SessionRepo for the App

instance AuthRepo App where
  addAuth             = PG.addAuth
  setEmailAsVerified  = PG.setEmailAsVerified
  findUserByAuth      = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession            = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

-- Let's have an example

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app = bracket createLogEnv closeScribes app
 where
  createLogEnv = do
    logEnv       <- initLogEnv "HAuth" "prod"
    stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

runExample :: IO ()
runExample = withKatip $ \le -> do
  mState <- newTVarIO M.initialState
  PG.withState pgcfg (\pgState -> run le (pgState, mState) action)
 where
  pgcfg = PG.Config { PG.cfgUrl = "postgresql://localhost/hauth"
                    , PG.cfgStripeCount          = 2
                    , PG.cfgMaxOpenConnPerStripe = 5
                    , PG.cfgIdleConnTimeout      = 10
                    }


action :: App ()
action = do
  let email = either undefined id $ mkEmail "felbit@example.com"
      passw = either undefined id $ mkPassword "SUP3rS3crEtP4ssW0Rd"
      auth  = Auth email passw
  register auth
  -- The repetetive pattern following is due to GHC-8.6's default MonadFailDesugaring
  -- TODO: instance of MonadFail for App
  mayVCode <- M.getNotificationsForEmail email
  vCode    <- case mayVCode of
    Nothing     -> error "Verification code not found."
    Just vCode' -> return vCode'
  verifyEmail vCode
  eitherSession <- login auth
  session       <- case eitherSession of
    Left  lsession' -> return "0"
    Right rsession  -> return rsession
  mayUId <- resolveSessionId session
  uId    <- case mayUId of
    Nothing   -> error "User not found."
    Just uId' -> return uId'
  mayRegEmail <- getUserEmail uId
  regEmail    <- case mayRegEmail of
    Nothing      -> error "Registration Mail not found."
    Just remail' -> return remail'
  print (session, uId, regEmail)
