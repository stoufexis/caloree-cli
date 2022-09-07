module Model.CustomFoodPreview
  ( CustomFoodPreview
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Model.Types                    ( Description
                                                , Id
                                                )
import           Model.User                     ( User )

data CustomFoodPreview = CustomFoodPreview
  { id          :: Id CustomFoodPreview
  , userId      :: Id User
  , description :: Description
  }
  deriving (Show, Generic)

instance ToJSON CustomFoodPreview
instance FromJSON CustomFoodPreview

