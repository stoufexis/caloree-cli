module Model.CustomFood
  (CustomFood(..)) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Model.Nutrients                ( Nutrients )
import Model.Types (Id, Description, Amount)
import Model.User (User)

data CustomFood = CustomFood
  { id          :: Id CustomFood
  , userId      :: Id User
  , description :: Description
  , amount      :: Amount
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON CustomFood
instance FromJSON CustomFood

