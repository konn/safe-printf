{-# LANGUAGE QuasiQuotes #-}
module Main where
import Text.Printf.Safe (fmt, printf)

main = do
  putStrLn $ printf [fmt|1 + 2 = %d and 0 == 1 is %S.|] (1 + 2) (0 == 1)
  putStrLn $ printf [fmt|42 is %010b in binary.|] 42
  putStrLn $ printf [fmt|48%% of people answers that the negation of True is %{show . not}.|] True
