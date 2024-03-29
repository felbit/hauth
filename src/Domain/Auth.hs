module Domain.Auth
    (
    -- * Types
      Auth(..)
    , Email
    , mkEmail
    , rawEmail
    , Password
    , mkPassword
    , rawPassword
    , UserId
    , VerificationCode
    , SessionId
    , RegistrationError(..)
    , EmailVerificationError(..)
    , LoginError(..)

    -- * Ports
    , AuthRepo(..)
    , SessionRepo(..)
    , EmailVerificationNotif(..)

    -- * Use cases
    , register
    , verifyEmail
    , login
    , resolveSessionId
    , getUserEmail
    )
where

import           ClassyPrelude
import           Control.Monad.Except
import           Domain.Validation
import           Katip
import           Text.Regex.PCRE.Heavy

-- * Types

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq, Ord)

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

type VerificationCode = Text

-- Could have as well been a `newtype UserId = UserId Int` for clarification
-- but that might have become combersome to use. Maybe I might change that with
-- the introduction of another ~Id Int (e.g. OrderId) ...
type UserId = Int

type SessionId = Text

data RegistrationError =
    RegistrationErrorEmailTaken deriving (Show, Eq)

data LoginError = LoginErrorInvalidAuth
                | LoginErrorEmailNotVerified
                deriving (Show, Eq)

data EmailVerificationError =
    EmailVerificationErrorInvalidCode deriving (Show, Eq)

data Auth =
    Auth { authEmail :: Email
         , authPassword :: Password
         } deriving (Show, Eq)

-- ** Ports for storage and notification interaction

{- Monadic Typeclasses

`AuthRepo` and `SessionRepo`contain functions for interaction with the
data repositories.
`EmailVerificationNotif` represents the notification system
(will be sending notification mails to the user).

These typeclasses are constrained by `Monad` since the specified operations
obviously have side effects.

-}

class (Monad m) => AuthRepo m where
    -- addAuth and setEmailAsVerified return 'UserId' for the purpose of logging
    addAuth :: Auth
            -> m (Either RegistrationError (UserId, VerificationCode))
    setEmailAsVerified :: VerificationCode
                       -> m (Either EmailVerificationError (UserId, Email))
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
    findEmailFromUserId :: UserId -> m (Maybe Email)

class (Monad m) => SessionRepo m where
    newSession :: UserId -> m SessionId
    findUserIdBySessionId :: SessionId -> m (Maybe UserId)

class (Monad m) => EmailVerificationNotif m where
    notifyEmailVerification :: Email
                            -> VerificationCode
                            -> m ()

-- * Functions

rawEmail :: Email -> Text
rawEmail = emailRaw

emailRegex :: Regex
emailRegex = [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email [regexMatches emailRegex "not a valid email address"]

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate
    Password
    [ lengthBetween 9 99 "length should be between 8 and 99"
    , regexMatches [re|\d|]    "should contain a number"
    , regexMatches [re|[A-Z]|] "should contain an upper-case letter"
    , regexMatches [re|[a-z]|] "should contain a lower-case letter"
    ]

-- ** Registration

{-| Adds an Auth to the repository through 'addAuth' which returns a
'VerificationCode'. Then notifies the user for email verification. ExceptT
ensures that the lines that follow will not be evaluated, if 'addAuth' returns
a Left value.
-}
register
    :: (KatipContext m, AuthRepo m, EmailVerificationNotif m)
    => Auth
    -> m (Either RegistrationError ())
register auth = runExceptT $ do
    (uId, vCode) <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode
    withUserIdContext uId
        $  $(logTM) InfoS
        $  ls (rawEmail email)
        <> " is registered successfully"


-- | Adds 'UserId' to the logging context
withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

{-| May seem like a useless indirection of `setEmailAsVerified` but keeps things
consistent and maintainable in the long run. This is also the point to extend
functionallity in verification of email in the future.

The very same for `resolveSessionId` and `getUserEmail`. These all are merely
indirections at the moment.
-}
verifyEmail
    :: AuthRepo m
    => VerificationCode
    -> m (Either EmailVerificationError (UserId, Email))
verifyEmail = setEmailAsVerified

-- ** Login

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
    result <- lift $ findUserByAuth auth
    case result of
        Nothing           -> throwError LoginErrorInvalidAuth
        Just (_  , False) -> throwError LoginErrorEmailNotVerified
        Just (uId, _    ) -> lift $ newSession uId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUserEmail :: AuthRepo m => UserId -> m (Maybe Email)
getUserEmail = findEmailFromUserId
