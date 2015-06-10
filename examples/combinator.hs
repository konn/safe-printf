{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Printf.Safe
import Text.Printf.Safe.Combinators (b', d, _S)

main :: IO ()
main = do
  putStrLn $ printf ("1 + 2 = " %d >< " and 0 == 1 is " %_S >< "." ) (1 + 2) (0 == 1)
  putStrLn $ printf ("42 is " % b' '0' 10 >< "in binary.") 42
  putStrLn $ printf ("48% of people answers that the negation of True is " %(show . not) >< ".") True
