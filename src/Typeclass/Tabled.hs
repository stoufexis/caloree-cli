{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Typeclass.Tabled
  ( Tabled(..)
  ) where
import           Colonnade                      ( Colonnade
                                                , Headed
                                                , ascii
                                                )
import           Data.Vector                    ( Vector
                                                , indexed
                                                )
import           Model.Types                    ( Verbosity )

class Tabled a where
  colonnade :: Verbosity -> Colonnade Headed (Int, a) String
  table :: Verbosity -> Vector a -> String

  default table :: Verbosity -> Vector a -> String
  table v = ascii (colonnade v) . indexed

