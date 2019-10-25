module Domain.Auth where

import           ClassyPrelude
import           Domain.Validation
import           Text.Regex.PCRE.Heavy
import           Control.Monad.Except

-- * Types

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq)

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

type VerificationCode = Text

data RegistrationError = RegistrationErrorEmailTaken deriving (Show, Eq)

data Auth = Auth { authEmail :: Email
                 , authPassword :: Password
                 } deriving (Show, Eq)

-- ** Ports for storage and notification interaction

{- Monadic Typeclasses

`AuthRepo` contains functions for interaction with the authentication
repository. `EmailVerificationNotif` represents the notification system
(will be sending notification mails to the user). Both typeclasses are
constrained by `Monad` since these operations will have side effects.

-}

class Monad m => AuthRepo m where
    addAuth :: Auth -> m (Either RegistrationError VerificationCode)

class Monad m => EmailVerificationNotif m where
    notifyEmailVerification :: Email -> VerificationCode -> m ()

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
    [ lengthBetween 12 99 "length should be between 12 and 99"
    , regexMatches [re|\d|]    "should contain a number"
    , regexMatches [re|[A-Z]|] "should contain an upper-case letter"
    , regexMatches [re|[a-z]|] "should contain a lower-case letter"
    ]

-- ** Registration

instance AuthRepo IO where
    addAuth (Auth email pass) = do
        putStrLn $ "adding auth > " <> rawEmail email
        return $ Right "fake verification code"

instance EmailVerificationNotif IO where
    notifyEmailVerification email vcode =
        putStrLn $ "Notify > " <> rawEmail email <> " - " <> vcode

{- register

Adds an Auth to the repository through `addAuth` which returns a
`VerificationCode`. Then notifies the user for email verification.
ExceptT ensures that the lines that follow will not be evaluated, if `addAuth`
returns a `Left` value.

-}

register
    :: (AuthRepo m, EmailVerificationNotif m)
    => Auth
    -> m (Either RegistrationError ())
register auth = runExceptT $ do
    vCode <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode
