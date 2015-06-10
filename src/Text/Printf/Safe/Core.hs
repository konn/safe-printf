{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}
module Text.Printf.Safe.Core (type (~>), Formatter, Printf(..),
                              HList(..), printf, printf') where
import Data.String (IsString (..))

-- | Variadic function types.
type family (~>) as b where
  (~>) '[] a = a
  (~>) (x ': xs) a = x -> xs ~> a

-- | Formatter type.
type Formatter a = a -> String

-- | Printf Format.
data Printf xs where
  EOS   :: Printf '[] -- ^ End of String.
  (:<>) :: String -> Printf xs -> Printf xs -- ^ Concat string
  (:%)  :: Formatter x -> Printf xs -> Printf (x ': xs) -- ^ Concat format function

instance (xs ~ '[]) => IsString (Printf xs) where
  fromString str = str :<> EOS

-- | Hetero list.
data HList ts where
  HNil :: HList '[]
  (:-) :: a -> HList xs -> HList (a ': xs)

infixr 9 :-, :<>

-- | HList version.
printf' :: Printf ts -> HList ts -> String
printf' ps0 ts0 = go ps0 ts0 ""
  where
    go :: Printf us -> HList us -> ShowS
    go EOS          HNil      = id
    go (str :<> fs) xs        = showString str . go fs xs
    go (fm  :% fs)  (x :- ds) = showString (fm x) . go fs ds
    go _ _ = error "bug in GHC!"

-- | Variadic version.
printf :: Printf xs -> xs ~> String
printf p = go p ""
  where
    go :: Printf xs -> String -> xs ~> String
    go EOS a          = a
    go (str :<> xs) a = go xs (a ++ str)
    go (fmt :% xs)  a = \x -> go xs (a ++ fmt x)

