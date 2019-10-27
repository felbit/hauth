# Authentication and Session Management in Haskell

> Pedo mellon a minno.

## Current Implementation State

Currently the domain code is implemented. That is `Domain.Validation` for validation
functionality for passwords and email addresses and `Domain.Auth` as main
authentication module.

Additionally there is a basic in-memory database that holds authentication and session
information.

## Test it

Clone or download the repository and enter it:

```
git clone https://github.com/felbit/hauth.git && cd hauth
```

Build the package and enter the REPL:

```
$ stack build
$ stack ghci --only-main
Ok, one module loaded.
λ>
```

Load the module: `Adapter.InMemory.Auth` which will include `Domain.Auth` and
`Domain.Validation`. With that in your REPL you can add a new authentication:

```
λ> let email = D.mkEmail "felbit@example.com"
λ> let passw = D.mkPassword "SeCreTP4ssw0rd"
λ> let auth = either undefined id $ D.Auth <$> email <*> passw
λ> s <- newTVarIO inititalState
λ> addAuth s auth
Right "hBdaG453DfaE42kN"
λ> findUserByAuth s auth
Just (1,False)
λ> findEmailFromUserId s 1
Just (Email {rawEmail = "felbit@example.com"})
λ> newSession s 1
"1hdnu28DHI89Hbdi2"
λ> findUserBySessionId s "1hdnu28DHI89Hbdi2"
Just 1
```

This is not very satisfying at the moment. I am working on a better frontend.

## Documentation

Generate [Haddock](https://www.haskell.org/haddock) documentation with `stack haddock`.

## Login Requirements

A user ...
* ... should be able to log in with email address and password
* ... should *not* be able to log in with invalid email address and password combination
* ... should *not* be able to log in if the email address has not been validated
