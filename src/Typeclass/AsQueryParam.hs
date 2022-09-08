module Typeclass.AsQueryParam
  ( AsQueryParam(..)
  ) where
import           Network.HTTP.Req               ( Option )

class AsQueryParam a where
  qparam :: a -> Option scheme

