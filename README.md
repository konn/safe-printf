safe-printf -- Well-typed, variadic and flexible printf functions for Haskell
=============================================================================

[![Build Status](https://travis-ci.org/konn/safe-printf.svg?branch=master)](https://travis-ci.org/konn/safe-printf) 
[![loop-effin](http://img.shields.io/hackage/v/safe-printf.svg)](http://hackage.haskell.org/package/safe-printf)

## What is this?
Haskell's standard `Text.Printf` module provides variadic `printf` function but not type-safe.
This library provides an alternative for this, more type-safe version of `printf` function,
combinators and quasiquoters.

The current implementation is just a proof-of-concept, so it is not so efficient and provides
APIs only for `String` value generation. In future, we will support `Text`
types and improve the effiiciency.

## Install
```sh
$ git clone https://github.com/konn/safe-printf.git
$ cd safe-printf
$ cabal install
```

<!--
```sh
$ cabal install safe-printf
```
-->

## Usage
We provide two interfaces to construct format: smart constructors and quasiquoters.

### Smart constructors
You need `OverloadedStrings` extension.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Printf.Safe             (printf, (%), (><))
import Text.Printf.Safe.Combinators (b', d, _S)
main = do
  putStrLn $ printf ("1 + 2 = " %d >< " and 0 == 1 is " %_S >< "." ) (1 + 2) (0 == 1)
  putStrLn $ printf ("42 is " % b' '0' 10 >< "in binary.") 42
  putStrLn $ printf ("48% of people answers that the negation of True is" %(show . not) >< ".") True
```

### Quasiquote interface
Quiasiquote interface provides more readable way for generating formats.

```haskell
{-# LANGUAGE QuasiQuotes #-}
module Main where
import Text.Printf.Safe    (printf, fmt)
main = do
  putStrLn $ printf [fmt|1 + 2 = %d and 0 == 1 is %S.|] (1 + 2) (0 == 1)
  putStrLn $ printf [fmt|42 is %010b in binary.|] 42
  putStrLn $ printf [fmt|48%% of people answers that the negation of True is %{show . not}.|] True
```

## TODO
* Support `Text` and perhaps `Builder`.
* Improve efficiency.
* Provide IO functions?

## Licence

BSD3

## Copyright

(c) Hiromi ISHII 2015
