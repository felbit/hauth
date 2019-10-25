module Domain.Validation where

import ClassyPrelude
import Text.Regex.PCRE.Heavy

{- Validation Type

`Validation` is a synonym for function that receives any inout and returns a
`Maybe` of any error message e. It returns `Nothing` if the input is valid.

-}
type Validation e a = a -> Maybe e

-- * Functions

{- validate Function

 - (a -> b): a construction definition which will be called if validation passes
 - [Validation a]: a list of validation functions
 - a: the value to be validated

The `concatMap` function applies each validation to `val` and concatenates the
results. If the validations return no errors that application will result in an
empty list.

-}
validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
  case concatMap (\f -> maybeToList $ f val) validations of
    []   -> Right $ constructor val
    errs -> Left errs

-- ** Validation Functions

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRange msg val =
  if val >= minRange && val <= maxRange then Nothing else Just msg

lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLength maxLength msg val =
  rangeBetween minLength maxLength msg (length val)

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val =
  if val =~ regex then Nothing else Just msg

