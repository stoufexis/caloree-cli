module Model.CustomFoodPreview
  ( CustomFoodPreview
  ) where
import           Control.Arrow                  ( Arrow(second) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Profunctor                ( Profunctor(lmap) )
import           GHC.Generics                   ( Generic )
import           Model.FoodPreview
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
  colonnade v = lmap (second toFood) $ colonnade v
   where
    toFood (CustomFoodPreview { id = i, description }) =
      FoodPreview { id = i, description }
