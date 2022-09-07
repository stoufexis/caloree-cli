module Dto.CustomFoodDto
  ( CustomFoodDto(..)
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Model.Nutrients                ( Nutrients )
import           Model.Types                    ( Description )

data CustomFoodDto = CustomFoodDto
  { description :: Description
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance FromJSON CustomFoodDto
instance ToJSON CustomFoodDto
