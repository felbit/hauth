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

data State = State
  { stateAuth :: [ (D.UserId, D.Auth) ]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails :: Set D.Email
  , stateUserIdCounter :: Int
  , stateNotifications :: Map D.Email D.VerificationCode
  , stateSessions :: Map D.SessionId D.UserId
  } deriving (Show, Eq)
