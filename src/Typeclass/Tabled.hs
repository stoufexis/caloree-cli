{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Typeclass.Tabled
  ( Tabled(..)
  ) where
import           Model.Types                    ( Verbosity )
import Data.Vector (Vector)

class Tabled a where
  table :: Verbosity -> Vector a -> String

