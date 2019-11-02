# Authentication and Session Management in Haskell

>
> Pedo mellon a minno.
>

## Current state of implementation

Currently the domain code is finished. That is `Domain.Validation` for validation
functionality for passwords and email addresses and `Domain.Auth` as main
authentication module.

I started adding adapters by implementing a basic in-memory _"database"_ that holds
authentication and session information.

I am currently working on a PostgreSQL adapter to give the storing of data some sense.

Also there's a basic logging in place, useing [Katip](https://github.com/Soostone/katip).

## Implementation

How to use the authentication module with in-memory database is demonstrated in
`src/Lib.hs`. The `action` function demonstrates application (prints a tupel of
`(SessionId, UserId, Email)`, e.g. `("1XSq9I1V7RUvO6zFr",1,Email {emailRaw =
"felbit@example.com"})`):

```
action = do
  let email = either undefined id $ mkEmail "felbit@example.com"
      passw = either undefined id $ mkPassword "SUP3rS3crEtP4ssW0Rd"
      auth  = Auth email passw
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session  <- login auth
  Just  uId      <- resolveSessionId session
  Just  regEmail <- getUserEmail uId
  print (session, uId, regEmail)
```

### GHC 8.6 and `Monad.Fail`

Currently `action` of `Lib.hs` has the somewhat ugly and repetitive pattern of

```
mayFoo <- M.getFoo x
foo    <- case mayFoo of
  Nothing     -> error "Foo not found."
  Just foo' -> return foo'
```

That is a quick workaround for the changed behaviour of `MonadFail` in GHC 8.6. I should
have implemented an `instance Fail.MonadFail App` here, and maybe I will do so later (read:
_when I did fully understand what I need to do to implement that_ ...); but for now I ran
with the quick-fix here.

## Logging

State changes are logged:

- User registration
- User login
- Email verification

## Documentation

Generate [Haddock](https://www.haskell.org/haddock) documentation with `stack haddock`. I will add this in the future as static site to this repository.

## Requirement Sketch

A user ...
* ... should be able to log in with email address and password
* ... should *not* be able to log in with invalid email address and password combination
* ... should *not* be able to log in if the email address has not been validated
