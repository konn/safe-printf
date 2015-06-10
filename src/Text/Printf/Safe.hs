{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleInstances, GADTs #-}
{-# LANGUAGE OverloadedStrings, PolyKinds, ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, TypeOperators      #-}
{-# LANGUAGE UndecidableInstances                                 #-}
module Text.Printf.Safe (module Text.Printf.Safe.Core,
                         module Text.Printf.Safe.QQ,
                         module Text.Printf.Safe.Combinators) where
import Text.Printf.Safe.Combinators ((%), (><))
import Text.Printf.Safe.Core
import Text.Printf.Safe.QQ

