# call-haskell-from-anything changelog

## 1.0.0.0 -- 2015-01-31

* Use closed type families to remove need for `Identity` monad at the end of functions.
  **You can now export any pure function of type `a -> b -> ... -> r`**.
  This is what I've been waiting for for years. Fixes #9.
* Remove all helpers, wrappers, and support for exporting functions of type `a -> b -> ... -> Identity r`. If you need those for some reasons, just `runIdentity` on them before passing them to `export`.

## 0.2.0.0 -- 2015-01-31

* Update to `msgpack-1.0.0`. Fixes #5.
* Fix compilation under GHC 7.10
* Require GHC >= 7.10
* Remove support for compilers that don't support `DataKinds`
* `stack` support.
* With `msgpack-1.0.0` we unfortunately lose the ability to give clear messages; if there is a number-of-arguments mismatch, `unpack` now returns `Nothing`.
