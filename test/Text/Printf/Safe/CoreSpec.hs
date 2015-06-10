{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Text.Printf.Safe.CoreSpec (main, spec) where
import Control.Exception     (ErrorCall (..))
import Control.Exception     (evaluate)
import Data.List             (isSuffixOf)
import Test.Hspec
import Text.Printf.Safe.Core

main :: IO ()
main = hspec spec

typeError :: Selector ErrorCall
typeError (ErrorCall exc) = "(deferred type error)" `isSuffixOf` exc

spec :: Spec
spec = do
  describe "printf" $ do
    it "preserves concat order" $ do
      printf ("This is just reproduction" :<> EOS) `shouldBe` "This is just reproduction"
      printf ("This is " :<> (show :: Int -> String) :% ".") 12
        `shouldBe` "This is 12."
      printf ("12 = 42 == " :<> show :% ".\n") ((12 :: Int) == 42)
        `shouldBe` "12 = 42 == False.\n"
      printf ("12 = 42 == " :<> show :% ".\n") ((12 :: Int) == 42)
        `shouldBe` "12 = 42 == False.\n"
    it "rejects too much arguments" $ do
      evaluate (printf "just plain text" ())
        `shouldThrow` typeError
      evaluate (printf "just plain text" () True)
        `shouldThrow` typeError
      evaluate (printf ("just plain text with num: " :<> (show :: Int -> String) :% EOS) (12 :: Int) ())
        `shouldThrow` typeError
    it "rejects too few arguments" $ do
      evaluate (printf ("just plain text with num: " :<> (show :: Int -> String) :% EOS :: String))
        `shouldThrow` typeError
    it "rejects ill-typed arguments" $ do
      evaluate (printf "just plain text with num: %d" ())
        `shouldThrow` typeError
