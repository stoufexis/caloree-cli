module Model.User
  ( User(..)
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Model.Nutrients                ( Nutrients )
import           Model.Types                    ( Id )

data User = User
  { id        :: Id
  , username  :: String
  , nutrients :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON User
instance FromJSON User
