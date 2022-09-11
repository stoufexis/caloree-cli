module Model.User
  ( User(..)
  ) where
import           GHC.Generics                   ( Generic )
import           Model.Nutrients                ( Nutrients )

data User = User
  { username  :: String
  , nutrients :: Nutrients
  }
  deriving (Show, Generic)
