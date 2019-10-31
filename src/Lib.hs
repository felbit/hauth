module Lib where

import qualified Adapter.InMemory.Auth         as M
import           ClassyPrelude
import           Domain.Auth
import           Katip

type State = TVar M.State

newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO
             , KatipContext, Katip)

run :: LogEnv -> State -> App a -> IO a
run le state = runKatipContextT le () mempty . flip runReaderT state . unApp

-- Glueing together the in memory and the domain logic with instances of
-- AuthRepo, EmailVerificationNotif and SessionRepo for the App

instance AuthRepo App where
  addAuth             = M.addAuth
  setEmailAsVerified  = M.setEmailAsVerified
  findUserByAuth      = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

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
  state <- newTVarIO M.initialState
  run le state action

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
