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
import Data.Text (Text)
import qualified Data.Text as T

class Tabled a where
  colonnade :: Verbosity -> Colonnade Headed (Int, a) String
  table :: Verbosity -> Vector a -> Text

  default table :: Verbosity -> Vector a -> Text
  table v = T.pack . ascii (colonnade v) . indexed

