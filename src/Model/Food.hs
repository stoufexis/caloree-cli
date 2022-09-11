{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Food
  ( Food(..)
  ) where
import           Colonnade                      ( ascii
                                                , headed
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Vector                   as V
import           Data.Vector                    ( indexed )
import           Fmt                            ( pretty )
import           GHC.Generics                   ( Generic )
import           Model.DateTime                 ( formatted )
import           Model.Nutrients                ( Nutrients(..) )
import           Model.Types
import           Typeclass.Tabled               ( Tabled(..) )

data Food = Food
  { id          :: Id
  , description :: Description
  , grams       :: Grams
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON Food
instance FromJSON Food

instance Tabled Food where
  colonnade v = mconcat
    [ headed "#" $ pretty . fst
    , headed "id" $ pretty . formatted . Model.Food.id . snd
    , headed "description" $ pretty . trimmed v . description . snd
    , headed "amount" $ pretty . formatted . grams . snd
    , headed "energy" (pretty . formatted . energy . nutrients . snd)
    ]

  table Minimal = pretty . formatted . Model.Food.id . V.head
  table v       = pretty . ascii (colonnade v) . indexed

