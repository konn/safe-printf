{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Text.Printf.Safe.CombinatorsSpec (main, spec) where

import Text.Printf.Safe             (Printf (..), printf)
import Text.Printf.Safe.Combinators

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

_b :: Bool -> String
_b = show

_u :: () -> String
_u = show

spec :: Spec
spec = do
  describe "(+++)" $ do
    prop "commutes with printf (for plain text)" $ \str str' ->
      let f1 = str  :<> EOS
          f2 = str' :<> EOS
      in printf (f1 +++ f2) == printf f1 ++ printf f2
    it "accumulates args in correct order" $
      let f1 = "john has " % d >< " dogs: " % _b
          f2 = "jane loves " % _u >< "."
      in printf (f1 +++ f2) (42 :: Int) False () == printf f1 42 False ++ printf f2 ()
  describe "numeric formatters" $ do
    it "works properly" $ do
      printf ("dec: " % d) (12 :: Int) `shouldBe` "dec: 12"
      printf ("pad: " % d' ' ' 4) (12 :: Int) `shouldBe` "pad:   12"
      printf ("pad: " % d' '0' 4) (12 :: Int) `shouldBe` "pad: 0012"

      printf ("oct: " % o) (12 :: Int) `shouldBe` "oct: 14"
      printf ("pad: " % o' ' ' 4) (12 :: Int) `shouldBe` "pad:   14"
      printf ("pad: " % o' '0' 4) (12 :: Int) `shouldBe` "pad: 0014"

      printf ("bin: " % b) (12 :: Int) `shouldBe` "bin: 1100"
      printf ("pad: " % b' ' ' 5) (12 :: Int) `shouldBe` "pad:  1100"
      printf ("pad: " % b' '0' 5) (12 :: Int) `shouldBe` "pad: 01100"

      printf ("hex: " % h) (12 :: Int) `shouldBe` "hex: c"
      printf ("pad: " % h' ' ' 4) (12 :: Int) `shouldBe` "pad:    c"
      printf ("pad: " % h' '0' 4) (12 :: Int) `shouldBe` "pad: 000c"

  describe "show formatters" $ do
    it "works properly" $ do
      printf ("w: " % _S) () `shouldBe` "w: ()"
      printf ("w: " % _S) True `shouldBe` "w: True"

  describe "string formatters" $ do
    it "works properly" $ do
      printf ("w: " % s) "hey!" `shouldBe` "w: hey!"
