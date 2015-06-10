module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Text/Printf/Safe/QQ.hs"]
