module Typeclass.Formatted
  ( Formatted(..)
  ) where
import Fmt (Builder)

class Formatted a where
  formatted :: a -> Builder
