# hauth

An authentification and authorization system for web applications written in Haskell.

## Documentation

Generate [Haddock](https://www.haskell.org/haddock) documentation with `stack haddock`.

## Login Requirements

A user ...
* ... should be able to log in with email address and password
* ... should *not* be able to log in with invalid email address and password combination
* ... should *not* be able to log in if the email address has not been validated

* ... will be redirected to login page if not authenticated
* ... will see his email address on a "profile page" otherwise