# Authentication and Session Management in Haskell

> 
> Pedo mellon a minno.
> 

## Current state of implementation

Currently the domain code is finished. That is `Domain.Validation` for validation
functionality for passwords and email addresses and `Domain.Auth` as main
authentication module.

Additionally there is a basic in-memory _"database"_ that holds authentication and session
information.

Next I will add a frontend to make using that stuff easier.

## Implementation

How to use the authentication module with in-memory database is demonstrated in `src/Lib.hs`. The `action` function demonstrates application (prints a tupel of `(SessionId, UserId, Email)`, e.g. `("1XSq9I1V7RUvO6zFr",1,Email {emailRaw = "felbit@example.com"})`):

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

## Documentation

Generate [Haddock](https://www.haskell.org/haddock) documentation with `stack haddock`. I will add this in the future as static site to this repository.

## Requirement Sketch

A user ...
* ... should be able to log in with email address and password
* ... should *not* be able to log in with invalid email address and password combination
* ... should *not* be able to log in if the email address has not been validated
