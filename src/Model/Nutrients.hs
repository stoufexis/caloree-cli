module Model.Nutrients
  ( Nutrients(..)
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )

data Nutrients = Nutrients
  { energy  :: Float
  , protein :: Float
  , carbs   :: Float
  , fat     :: Float
  , fiber   :: Float
  }
  deriving (Show, Generic)

instance ToJSON Nutrients
instance FromJSON Nutrients
