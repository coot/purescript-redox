# REDOX - global state management for PureScript apps

Redox - since it mixes well with [Thermite](https://github.com/paf31/purescript-thermite) ;)

This is `redux` type of store, but instead of forcing you to write interpreter
as a reducer you are free to write it the way you want even use different
schemes how to update it.  Now there is only one `Redox.DSL`, but in near future
there will be at least one more using coroutines (similar to how
[Thermite](https://github.com/paf31/purescript-thermite) updates react
component state. 

# Redox.DSL

Check out tests how to write a DSL using a `Free` monad and an interpreter
using `Cofree` comonad. The DSL has to be interpreted in the `Aff` monad.
Since `Aff` has an instance of `MonadEff` this does not restrict you in any
way.  Checkout tests how to write synchronous and asynchronous commands
