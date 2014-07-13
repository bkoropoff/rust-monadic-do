rust-monadic-do
===============

This library provides a macro inspired by Haskell monadic do notation
and "monad instances" for `Option` and `Result`.  Due to current limitations
on closures, many instances (e.g. for `Iterator`) are not possible.
