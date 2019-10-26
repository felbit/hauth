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
import qualified Domain.Auth                   as D
import           Data.Has

data State = State
  { stateAuth :: [ (D.UserId, D.Auth) ]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails :: Set D.Email
  , stateUserIdCounter :: Int
  , stateNotifications :: Map D.Email D.VerificationCode
  , stateSessions :: Map D.SessionId D.UserId
  } deriving (Show, Eq)

-- | This type alias is made possible through the "ConstraintKInds" language
-- extension. Without that I would have to write the full constraint in each
-- function like so:
--
-- >>> addAuth :: (Has (TVar State) r, MonadReader r m, MonadIO m)
--             => Auth -> m (Either RegistrationError VerificationCode)
type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

initialState :: State
initialState = State { stateAuth             = []
                     , stateUnverifiedEmails = mempty
                     , stateVerifiedEmails   = mempty
                     , stateUserIdCounter    = 0
                     , stateNotifications    = mempty
                     , stateSessions         = mempty
                     }

-- ** Repository functions (copied from Domain.Auth) with InMemory constraint.

addAuth
  :: InMemory r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth = undefined

setEmailAsVerified
  :: InMemory r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId = undefined

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession = undefined

findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar

notifyEmailVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification = undefined
