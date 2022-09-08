{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Model.CustomFood
  ( CustomFood(..)
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Profunctor                ( Profunctor(lmap) )
import           GHC.Generics                   ( Generic )
import           Model.Food                     ( Food(..) )
import           Model.Nutrients                ( Nutrients(..) )
import           Model.Types
import           Typeclass.Tabled               ( Tabled(..) )
import Control.Arrow (Arrow(second))

data CustomFood = CustomFood
  { id          :: Id
  , userId      :: Id
  , description :: Description
  , grams       :: Amount
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON CustomFood
instance FromJSON CustomFood

instance Tabled CustomFood where
  colonnade v = lmap (second toFood) $ colonnade v
   where
    toFood (CustomFood { id = i, description, grams, nutrients }) =
      Food { id = i, description, grams, nutrients }
