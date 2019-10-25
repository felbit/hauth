module Domain.Auth where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy

-- * Types

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq)

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

data RegistrationError = RegistrationErrorEmailTaken deriving (Show, Eq)

data Auth = Auth { authEmail :: Email
                 , authPassword :: Password
                 } deriving (Show, Eq)

-- * Functions

rawEmail :: Email -> Text
rawEmail = emailRaw

emailRegex :: Regex
emailRegex = [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
    [ regexMatches emailRegex "not a valid email address" ]

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
    [ lengthBetween 12 99 "length should be between 12 and 99"
    , regexMatches [re|\d|] "should contain a number"
    , regexMatches [re|[A-Z]|] "should contain an upper-case letter"
    , regexMatches [re|[a-z]|] "should contain a lower-case letter"
    ]
