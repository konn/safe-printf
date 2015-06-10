{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Text.Printf.Safe.QQ (fmt) where
import Language.Haskell.TH.Quote    (QuasiQuoter (..))
import Text.Printf.Safe.QQ.Internal (parse)

fmt :: QuasiQuoter
fmt = QuasiQuoter { quoteDec = error "not implemented"
                  , quoteType = error "not implemented"
                  , quotePat  = error "not implemented"
                  , quoteExp  = parse
                  }
