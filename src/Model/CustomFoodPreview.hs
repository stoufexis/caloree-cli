module Model.CustomFoodPreview
  ( CustomFoodPreview
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Vector                   as V
import           GHC.Generics                   ( Generic )
import           Model.FoodPreview              ( FoodPreview(..) )
import           Model.Types                    ( Description
                                                , Id
                                                )
import           Typeclass.Tabled               ( Tabled(..) )


data CustomFoodPreview = CustomFoodPreview
  { id          :: Id
  , userId      :: Id
  , description :: Description
  }
  deriving (Show, Generic)

instance FromJSON CustomFoodPreview
instance ToJSON CustomFoodPreview

instance Tabled CustomFoodPreview where
  table v = table v . V.map asFood
   where
    asFood (CustomFoodPreview { id, description }) =
      FoodPreview { id, description }

