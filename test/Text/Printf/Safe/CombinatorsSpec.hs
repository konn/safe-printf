{-# LANGUAGE TemplateHaskell #-}
module Text.Printf.Safe.CombinatorsSpec (main, spec) where

import Text.Printf.Safe             (Printf (..), printf)
import Text.Printf.Safe.Combinators

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "concat" $ do
    prop "commutes with printf (for plain text)" $ \str str' ->
      let f1 = str  :<> EOS
          f2 = str' :<> EOS
      in printf (f1 +++ f2) == printf f1 ++ printf f2
