{-|
Module      : Adapter.InMemory.Auth
Description : In memory database for authentication
Copyright   : (c) Martin Penckert, 2019
License     : BSD3
Maintainer  : martin.penckert@gmail.com
Stability   : experimental

The in memory database relies on state manipulation, which is something
that cannot (easily) be done in a purely functional language like Haskell. I
therefore use the Software Transactional Memory provided by package stm
and reexported by ClassyPrelude.

stm has a datatype called "TVar" that is in behaviour very similar to a
Clojure atom. It wraps an immutable value and is itself mutable; the wrapped
value can be replaced by another value and thus provides the possibility of
state change in place. That obviously needs a Monad.

-}
module Adapter.InMemory.Auth where

import           ClassyPrelude
import           Control.Monad.Except
import           Data.Has
import qualified Domain.Auth                   as D
import           Text.StringRandom

data State = State
  { stateAuths            :: [ (D.UserId, D.Auth) ]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails   :: Set D.Email
  , stateUserIdCounter    :: Int
  , stateNotifications    :: Map D.Email D.VerificationCode
  , stateSessions         :: Map D.SessionId D.UserId
  } deriving (Show, Eq)

-- | This type alias is made possible through the "ConstraintKInds" language
-- extension. Without that I would have to write the full constraint in each
-- function like so:
--
-- >>> addAuth :: (Has (TVar State) r, MonadReader r m, MonadIO m)
--             => Auth -> m (Either RegistrationError VerificationCode)
type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

initialState :: State
initialState = State { stateAuths            = []
                     , stateUnverifiedEmails = mempty
                     , stateVerifiedEmails   = mempty
                     , stateUserIdCounter    = 0
                     , stateNotifications    = mempty
                     , stateSessions         = mempty
                     }

-- ** Auth Repository implementation under InMemory constraint.

-- | Generates a new verification code (random 16 character string) similar to
-- 'newSession' and checks if the given email is a duplicate (returns
-- RegistrationErrorEmailTaken error in that case). If email is unique it is
-- added to stateAuths with a newly generated UserId (increments the
-- userId-counter). Adds the email and generated verification token to
-- stateUnverifiedEmails before returning with the verification code.
addAuth
  :: InMemory r m
  => D.Auth
  -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar  <- asks getter
  -- generate the verification code
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    -- check, whether the given email is a duplicate
    let auths       = stateAuths state
        email       = D.authEmail auth
        isDuplicate = any (email ==) . map (D.authEmail . snd) $ auths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    -- update state
    let newUserId      = stateUserIdCounter state + 1
        newAuths       = (newUserId, auth) : auths
        unverifieds    = stateUnverifiedEmails state
        newUnverifieds = insertMap vCode email unverifieds
        newState       = state { stateAuths            = newAuths
                               , stateUserIdCounter    = newUserId
                               , stateUnverifiedEmails = newUnverifieds
                               }
    lift $ writeTVar tvar newState
    return (newUserId, vCode)

-- | Throw the second parameter if the first parameter is Nothing.
orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing  e = throwError e
orThrow (Just a) _ = return a

-- | Looks up an Email in stateUnverifiedEmails via given VerificationCode and
-- moves that into stateVerifiedEmails. If the given VerificationCode does not
-- map to any Email it throws an EmailVerificationErrorInvalidCode error.
setEmailAsVerified
  :: InMemory r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar

    let unverifieds = stateUnverifiedEmails state
        mayEmail    = lookup vCode unverifieds
    email <- mayEmail `orThrow` D.EmailVerificationErrorInvalidCode

    let auths     = stateAuths state
        mayUserId = map fst . find ((email ==) . D.authEmail . snd) $ auths
    uId <- mayUserId `orThrow` D.EmailVerificationErrorInvalidCode

    let verifieds      = stateVerifiedEmails state
        newVerifieds   = insertSet email verifieds
        newUnverifieds = deleteMap vCode unverifieds
        newState       = state { stateUnverifiedEmails = newUnverifieds
                               , stateVerifiedEmails   = newVerifieds
                               }
    lift $ writeTVar tvar newState
    return (uId, email)

-- | Extracts the UserId from the Auth (return Nothing if no UserId is found).
-- With UserId check for verified Emails and return UserId and the current
-- verification status.
findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  let maybeUserId = map fst . find ((auth ==) . snd) $ stateAuths state
  case maybeUserId of
    Nothing  -> return Nothing
    Just uId -> do
      let verifieds  = stateVerifiedEmails state
          email      = D.authEmail auth
          isVerified = email `elem` verifieds
      return $ Just (uId, isVerified)

findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  let maybeAuth = map snd . find ((uId ==) . fst) $ stateAuths state
  return $ D.authEmail <$> maybeAuth

-- ** Session Repository implementation under InMemory constraint.

-- | Generates a unique random 16 chars string as "SessionId" and adds the
-- sessionId mapped to the given userId to in memory state database.
-- Adding uId on front of the sessionId string ensures a truly unique sessionId.
newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId  <- liftIO $ ((tshow uId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions    = stateSessions state
        newSessions = insertMap sId uId sessions
        newState    = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sId

-- | Looks for sessionId in the stateSessions structure.
findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar

-- ** Notification implementation under InMemory constraint.

-- | Inserts a given email address and the corresponding "VerificationCode" to
-- state's stateNotifications
notifyEmailVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let notifications    = stateNotifications state
        newNotifications = insertMap email vCode notifications
        newState         = state { stateNotifications = newNotifications }
    writeTVar tvar newState

-- | For testing purposes this function fakes the notification process since I
-- am not able to send actual email out (yet).
getNotificationsForEmail
  :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications state
