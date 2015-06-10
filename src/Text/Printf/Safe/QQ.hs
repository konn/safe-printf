{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Text.Printf.Safe.QQ (fmt) where
import Data.Char                 (intToDigit, isDigit, isUpper, toUpper)
import Data.Maybe                (fromMaybe)
import Language.Haskell.Meta     (parseExp)
import Language.Haskell.TH       (ExpQ)
import Language.Haskell.TH.Lift  (lift)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Numeric                   (showIntAtBase)
import Text.Printf.Safe.Core     (Printf (..))

fmt :: QuasiQuoter
fmt = QuasiQuoter { quoteDec = error "not implemented"
                  , quoteType = error "not implemented"
                  , quotePat  = error "not implemented"
                  , quoteExp  = parse
                  }

data Fragment = StrF  String
              | ResF  FormatterConf
              | AntiF String
                deriving (Read, Show, Eq, Ord)

data FormatterConf = Float
                   | Integral { base :: Integer, padding :: Maybe Char, digits :: Maybe Int, capital :: Bool}
                   | String
                   | Show
                   deriving (Read, Show, Eq, Ord)

parse :: String -> ExpQ
parse = foldr cat [|EOS|] . parse'
  where
    cat (StrF s)      r = [| $(lift s) :<> $r |]
    cat (ResF Float)  r = [| (show :: Real a => a -> String) :% $r |]
    cat (ResF String) r = [| (id :: String -> String) :% $r |]
    cat (ResF Show)   r = [| (show :: Show a => a -> String) :% $r |]
    cat (ResF Integral{..}) r = do
      let pad = lift $ fromMaybe ' ' padding
          wid = lift digits
      [| ((\str -> replicate (maybe 0 (subtract (length str)) $wid) $pad ++ str) .  flip $([| showIntAtBase $(lift base) ($(if capital then [|toUpper|] else [|id|]) . intToDigit) |]) "")  :% $r |]
    cat (AntiF code) r = [| $(return $ either error id $ parseExp code) :% $r |]

parseFormat :: String -> [Fragment]
parse' :: String -> [Fragment]
parse' str = case break (=='%') str of
  ("", "") -> []
  (as, "") -> [StrF as]
  (as, '%':'%':bs) -> StrF as : StrF "%" : parse' bs
  (as, '%':rest)   -> StrF as : parseFormat rest
  (as, bs) -> StrF as : parse' bs

parseFormat "" = [StrF "%"]
parseFormat ('{':r) = go (1 :: Int) "" r
  where
    go 0 c k       = AntiF (init c) : parse' k
    go p c ('{':k) = go (p+1) (c ++ "{") k
    go p c ('}':k) = go (p-1) (c ++ "}") k
    go p c (u:k)   = go p (c ++ [u]) k
    go _ _ [] = StrF "{" : parse' r
parseFormat str =
  case span isDigit str of
    ("", 's':rest) -> ResF String : parse' rest
    ("", 'S':rest) -> ResF Show : parse' rest
    ("", 'f':rest) -> ResF Float : parse' rest
    ("", r:rest) | Just base <- getBase r -> ResF (Integral base Nothing Nothing (isUpper r)) : parse' rest
    ('0':ds, r:rest) | Just base <- getBase r ->
      ResF (Integral base (Just '0') (Just $ read ds) (isUpper r)) : parse' rest
    (ds, r:rest) | Just base <- getBase r ->
      ResF (Integral base (Just ' ') (Just $ read ds) (isUpper r)) : parse' rest
    _ -> parse' str

getBase :: Char -> Maybe Integer
getBase 'd' = Just 10
getBase 'b' = Just 2
getBase 'o' = Just 8
getBase 'h' = Just 16
getBase 'H' = Just 16
getBase _ = Nothing
