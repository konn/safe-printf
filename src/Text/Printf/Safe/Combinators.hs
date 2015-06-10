{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
module Text.Printf.Safe.Combinators (-- * Smart constructors
                                     type (<>), (><), (%), (+++),
                                     -- * Basic formatters
                                     s, _S, _shows,
                                     -- ** Number Formatters
                                     base, d, d', o, o', b, b', h, h', f) where
import Data.Char             (intToDigit)
import Data.Type.Equality    ((:~:) (..), gcastWith)
import Numeric               (showFloat, showIntAtBase)
import Text.Printf.Safe.Core (Printf (..), Formatter)

-- * Smart constructors
type family (<>) xs ys where
  (<>) '[] xs = xs
  (<>) (x ': xs) ys = x ': (xs <> ys)

appPrf :: Printf ts -> Printf ps -> Printf (ts <> ps)
appPrf EOS ps = ps
appPrf (str :<> ts) ps = str :<> appPrf ts ps
appPrf (fm :% ts)   ps = fm :% appPrf ts ps

appNil :: Printf ts -> (ts <> '[]) :~: ts
appNil EOS = Refl
appNil (_ :<> a) = appNil a
appNil (_ :% bs) = case appNil bs of Refl -> Refl

(+++) :: Printf ts -> Printf ps -> Printf (ts <> ps)
(+++) = appPrf
infixr 5 +++

(><) :: Printf ts -> String -> Printf ts
xs >< str = gcastWith (appNil xs) $ appPrf xs (str :<> EOS)

(%) :: Printf ts -> (a -> String) -> Printf (ts <> '[a])
(%) xs p = appPrf xs (p :% EOS)

infixl 5 ><, %

-- | Format @String@ as it is.
s :: Formatter String
s = id

-- | Formatter for @Show@ instances.
_S :: Show a => Formatter a
_S = show

-- | Converts @'ShowS'@ function to @'Formatter'@.
_shows :: (a -> ShowS) -> Formatter a
_shows fmt = flip fmt ""

-- | Format @Integral@s at base.
base :: (Show a, Integral a)
     => a                       -- ^ Base
     -> Maybe (Char, Int)       -- ^ Padding settings.
     -> Formatter a
base adic mpd a =
  let ans = showIntAtBase adic intToDigit a ""
  in maybe "" (\(c,dig) -> replicate (dig - length ans) c) mpd ++ ans

-- | Decimal formatter with padding.
d' :: (Show a, Integral a) => Char -> Int -> Formatter a
d' c i = base 10 (Just (c,i))

-- | No padding version of @'d''@.
d :: (Show a, Integral a) => Formatter a
d = base 10 Nothing

-- | Octal formatter with padding.
o' :: (Show a, Integral a) => Char -> Int -> Formatter a
o' c i = base 8 (Just (c,i))

-- | No padding version of @'o''@.
o :: (Show a, Integral a) => Formatter a
o = base 8 Nothing

-- | Binary formatter with padding.
b' :: (Show a, Integral a) => Char -> Int -> Formatter a
b' c i = base 2 (Just (c,i))

-- | No padding version of @'b''@.
b :: (Show a, Integral a) => Formatter a
b = base 2 Nothing

-- | Binary formatter with padding.
h' :: (Show a, Integral a) => Char -> Int -> Formatter a
h' c i = base 16 (Just (c,i))

-- | No padding version of @'b''@.
h :: (Show a, Integral a) => Formatter a
h = base 16 Nothing

-- | @RealFloat@ formatter.
f :: RealFloat a => Formatter a
f = _shows showFloat
