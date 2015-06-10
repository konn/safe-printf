{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Text.Printf.Safe.QQ (fmt) where
import Language.Haskell.TH.Quote    (QuasiQuoter (..))
import Text.Printf.Safe.Core        (printf)
import Text.Printf.Safe.QQ.Internal (parse)

-- | Quasiquoter for formatter.
-- It supprots escape sequence.
-- Formatter is prefixed by @%@ and you can use @%{hoge}@ to antiquotation.
--
-- >>> :set -XQuasiQuotes
-- >>> printf [fmt|Answer is: %d and %S|] 42 True
-- "Answer is: 42 and True"
--
-- >>> printf [fmt|%02d%% of people answers %{show . not}.\n|] 4 False
-- "04% of people answers True.\n"
--
-- Predefined formatters:
--
-- [@%%@] outputs @%@.
-- [@%d@] formats @'Integral'@ value in decimal.
-- [@%/n/d@] same as above, but padding with @' '@ (space) to /n/ digits.
-- [@%0/n/d@] same as above, but padding with @'0'@ to /n/ digits.
-- [@%b@, @%o@, @%h@, @%H@] formats @'Integral'@s in binary, octet, hex and HEX resp.
--                          Padding options can be specified as @%d@.
-- [@%f@] formats @'Real'@ value with @'show'@ function.
-- [@%s@] embeds @'String'@ value.
-- [@%S@] embeds @'Show'@ instances.
-- [@%{/expr/}@, where @/expr/@ is a Haskell expression of type @a -> String@]
--    Antiquote. This formats corresponding argument by passing to @/expr/@.

fmt :: QuasiQuoter
fmt = QuasiQuoter { quoteDec = error "not implemented"
                  , quoteType = error "not implemented"
                  , quotePat  = error "not implemented"
                  , quoteExp  = parse
                  }
