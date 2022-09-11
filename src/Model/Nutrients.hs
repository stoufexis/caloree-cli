module Model.Nutrients
  ( Nutrients(..)
  ) where
import           Colonnade                      ( headed )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Fmt
import           GHC.Generics                   ( Generic )
import           Model.Types                    ( Verbosity(Verbose) )
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

instance Semigroup Nutrients where
  n <> n' = Nutrients (energy n + energy n')
                      (protein n + protein n')
                      (carbs n + carbs n')
                      (fat n + fat n')
                      (fiber n + fiber n')

instance Monoid Nutrients where
  mempty = Nutrients 0 0 0 0 0

instance Tabled Nutrients where
  colonnade v = mconcat
    [ headed "energy" $ present energy "kcal"
    , headed "protein" $ present protein "gr"
    , headed "carbs" $ present carbs "gr"
    , headed "fat" $ present fat "gr"
    , headed "fiber" $ present fiber "gr"
    ]
   where
    present f u = (|+ " " +| u) . rnd . f . snd

    rnd :: Float -> Text
    rnd n = case v of
      Verbose -> pretty n
      _       -> pretty (round n :: Integer)
