{-# LANGUAGE LambdaCase #-}
module Text.Printf.Safe.QQ.InternalSpec (main, spec) where
import Test.Hspec

import Control.Applicative          ((<$>))
import Data.Maybe                   (isJust)
import Test.Hspec.QuickCheck        (prop)
import Test.QuickCheck              ((==>))
import Text.Printf.Safe.QQ.Internal


stringify :: [Fragment] -> Maybe String
stringify fs = concat <$> sequence (map (\case {StrF s -> Just s; _ -> Nothing}) fs)

main :: IO ()
main = hspec spec

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
  [(a, "")] -> Just a
  _ -> Nothing

spec :: Spec
spec = do
  describe "parse'" $ do
    prop "should produce the same effect for strings without %" $ \str0 ->
      let str = filter (/= '%') str0
          ans = readMaybe ('"':str++"\"")
      in isJust ans ==> ans == stringify (parse' str)
    it "correctly parses numeral formats" $ do
      parse' "%23d" `shouldBe` [ResF Integral { base = 10
                                              , padding = Just ' '
                                              , digits = Just 23
                                              , capital = False}]
      parse' "%023d" `shouldBe` [ResF Integral { base = 10
                                               , padding = Just '0'
                                               , digits = Just 23
                                               , capital = False}]
      parse' "%023b" `shouldBe` [ResF Integral { base = 2
                                               , padding = Just '0'
                                               , digits = Just 23
                                               , capital = False}]
      parse' "%s" `shouldBe` [ResF String]
      parse' "%S" `shouldBe` [ResF Show]
      parse' "%f" `shouldBe` [ResF Float]
      parse' "%{n{o}t}" `shouldBe` [AntiF "n{o}t"]
    it "correctly processes percent escape" $ do
      parse' "%%s" `shouldBe` [StrF "%s"]
      parse' "%%{s" `shouldBe` [StrF "%{s"]
      parse' "%%%s" `shouldBe` [StrF "%", ResF String]


