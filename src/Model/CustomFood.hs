{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Model.CustomFood
  ( CustomFood(..)
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Vector                   as V
import           GHC.Generics                   ( Generic )
import           Model.Food                     ( Food(..) )
import           Model.Nutrients                ( Nutrients )
import           Model.Types                    ( Amount
                                                , Description
                                                , Id
                                                )
import           Typeclass.Tabled               ( Tabled(..) )

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
  table v = table v . V.map asFood
   where
    asFood (CustomFood { id, description, grams, nutrients }) =
      Food { id, description, grams, nutrients }
