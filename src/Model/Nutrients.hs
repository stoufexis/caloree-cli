module Model.Nutrients
  ( Nutrients(..)
  ) where
import           Colonnade                      ( headed )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Fmt                            ( pretty )
import           GHC.Generics                   ( Generic )
import           Typeclass.Tabled

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

instance Tabled Nutrients where
  colonnade _ = mconcat
    [ headed "energy" $ pretty . energy . snd
    , headed "protein" $ pretty . protein . snd
    , headed "carbs" $ pretty . carbs . snd
    , headed "fat" $ pretty . fat . snd
    , headed "fiber" $ pretty . fiber . snd
    ]
