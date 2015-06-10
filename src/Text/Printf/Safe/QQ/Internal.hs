{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Text.Printf.Safe.QQ.Internal where
import Control.Applicative      ((<$>))
import Data.Char                (intToDigit, isDigit, isUpper, toUpper)
import Data.Char                (chr)
import Data.Maybe               (fromMaybe)
import Language.Haskell.Meta    (parseExp)
import Language.Haskell.TH      (ExpQ)
import Language.Haskell.TH.Lift (lift)
import Numeric                  (showIntAtBase)
import Numeric                  (readDec, readHex, readOct)
import Text.Printf.Safe.Core    (Printf (..))

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

parse' :: String -> [Fragment]
parse' = either error (foldr cat []) . parse''
  where
    cat (StrF "") xs = xs
    cat x [] = [x]
    cat (StrF x) (StrF y : xs) = cat (StrF (x ++ y)) xs
    cat x xs = x : xs

parse'' :: String -> Either String[Fragment]
parse'' str = case break (`elem`"%\\") str of
  ("", "") -> return []
  (as, "") -> return [StrF as]
  (as, '\\':bs) -> case parseEscape bs of
    Just (ch, rest) -> (StrF (as ++ ch) : ) <$> parse'' rest
    Nothing -> Left $ "illeagual escape sequence: \\" ++ (take 1 bs)
  (as, '%':'%':bs) -> (StrF as :) . (StrF "%" :) <$> parse'' bs
  (as, '%':rest)   ->
    let (pc, bs) = parseFormat rest
    in (StrF as :) . (pc:) <$> parse'' bs
  (as, bs) -> (StrF as :) <$> parse'' bs

parseEscape :: String -> Maybe (String, String)
parseEscape ('a':rest)  = Just ("\a", rest)
parseEscape ('b':rest)  = Just ("\b", rest)
parseEscape ('f':rest)  = Just ("\f", rest)
parseEscape ('n':rest)  = Just ("\n", rest)
parseEscape ('r':rest)  = Just ("\r", rest)
parseEscape ('t':rest)  = Just ("\t", rest)
parseEscape ('v':rest)  = Just ("\v", rest)
parseEscape ('\\':rest) = Just ("\\", rest)
parseEscape (']':rest) = Just ("]", rest)
parseEscape ('&':rest) = Just ("", rest)
parseEscape ('"':rest) = Just ("\"", rest)
parseEscape ('\'':rest) = Just ("\'", rest)
parseEscape ('N':'U':'L':rest) = Just ("\NUL", rest)
parseEscape ('S':'O':'H':rest) = Just ("\SOH", rest)
parseEscape ('S':'T':'X':rest) = Just ("\STX", rest)
parseEscape ('E':'T':'X':rest) = Just ("\ETX", rest)
parseEscape ('E':'O':'T':rest) = Just ("\EOT", rest)
parseEscape ('E':'N':'Q':rest) = Just ("\ENQ", rest)
parseEscape ('A':'C':'K':rest) = Just ("\ACK", rest)
parseEscape ('B':'E':'L':rest) = Just ("\BEL", rest)
parseEscape ('B':'S':rest) = Just ("\BS", rest)
parseEscape ('H':'T':rest) = Just ("\HT", rest)
parseEscape ('L':'F':rest) = Just ("\LF", rest)
parseEscape ('V':'T':rest) = Just ("\VT", rest)
parseEscape ('F':'F':rest) = Just ("\FF", rest)
parseEscape ('C':'R':rest) = Just ("\CR", rest)
parseEscape ('S':'O':rest) = Just ("\SO", rest)
parseEscape ('S':'I':rest) = Just ("\SI", rest)
parseEscape ('D':'L':'E':rest) = Just ("\DLE", rest)
parseEscape ('D':'C':'1':rest) = Just ("\DC1", rest)
parseEscape ('D':'C':'2':rest) = Just ("\DC2", rest)
parseEscape ('D':'C':'3':rest) = Just ("\DC3", rest)
parseEscape ('D':'C':'4':rest) = Just ("\DC4", rest)
parseEscape ('N':'A':'K':rest) = Just ("\NAK", rest)
parseEscape ('S':'Y':'N':rest) = Just ("\SYN", rest)
parseEscape ('E':'T':'B':rest) = Just ("\ETB", rest)
parseEscape ('C':'A':'N':rest) = Just ("\CAN", rest)
parseEscape ('E':'M':rest) = Just ("\EM", rest)
parseEscape ('S':'U':'B':rest) = Just ("\SUB", rest)
parseEscape ('E':'S':'C':rest) = Just ("\ESC", rest)
parseEscape ('F':'S':rest) = Just ("\FS", rest)
parseEscape ('G':'S':rest) = Just ("\GS", rest)
parseEscape ('R':'S':rest) = Just ("\RS", rest)
parseEscape ('U':'S':rest) = Just ("\US", rest)
parseEscape ('S':'P':rest) = Just ("\SP", rest)
parseEscape ('D':'E':'L':rest) = Just ("\DEL", rest)
parseEscape ('^':c:rest)
  | c `elem` ['A'..'Z'] ++ "@[]\\^_" =
    Just ([read ("'\\^" ++ c : "'")], rest)
parseEscape ('x':bs)
  | (ds, rest) : _ <- readHex bs = Just ([chr ds], rest)
parseEscape ('o':bs)
  | (ds, rest) : _ <- readOct bs = Just ([chr ds], rest)
parseEscape bs
  | (ds, rest) : _ <- readDec bs = Just ([chr ds], rest)
parseEscape _ = Nothing

parseFormat :: String -> (Fragment, String)
parseFormat "" = (StrF "%", "")
parseFormat ('{':r) = go (1 :: Int) "" r
  where
    go 0 c k       = (AntiF (init c), k)
    go p c ('{':k) = go (p+1) (c ++ "{") k
    go p c ('}':k) = go (p-1) (c ++ "}") k
    go p c (u:k)   = go p (c ++ [u]) k
    go _ _ [] = (StrF "{", r)
parseFormat str =
  case span isDigit str of
    ("", 's':rest) -> (ResF String, rest)
    ("", 'S':rest) -> (ResF Show, rest)
    ("", 'f':rest) -> (ResF Float, rest)
    ("", r:rest) | Just base <- getBase r ->
      (ResF (Integral base Nothing Nothing (isUpper r)), rest)
    ('0':ds, r:rest) | Just base <- getBase r ->
      (ResF (Integral base (Just '0') (Just $ read ds) (isUpper r)), rest)
    (ds, r:rest) | Just base <- getBase r ->
      (ResF (Integral base (Just ' ') (Just $ read ds) (isUpper r)), rest)
    _ -> (StrF "%", str)

getBase :: Char -> Maybe Integer
getBase 'd' = Just 10
getBase 'b' = Just 2
getBase 'o' = Just 8
getBase 'h' = Just 16
getBase 'H' = Just 16
getBase _ = Nothing
