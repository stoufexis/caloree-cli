module Model.Nutrients
  ( Nutrients(..)
  ) where
import           Colonnade                      ( headed )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Fmt
import           GHC.Generics                   ( Generic )
import           Model.DateTime                 ( formatted )
import           Model.Types                    ( Grams(Grams)
                                                , Kcal(Kcal)
                                                , Verbosity(Verbose)
                                                )
import           Typeclass.Tabled



data Nutrients = Nutrients
  { energy  :: Kcal
  , protein :: Grams
  , carbs   :: Grams
  , fat     :: Grams
  , fiber   :: Grams
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
    [ headed "energy" $ presentKcal . energy . snd
    , headed "protein" $ presentGrams . protein . snd
    , headed "carbs" $ presentGrams . carbs . snd
    , headed "fat" $ presentGrams . fat . snd
    , headed "fiber" $ presentGrams . fiber . snd
    ]
   where
    presentGrams (Grams g) = pretty . formatted $ Grams $ rnd g
    presentKcal (Kcal k) = pretty . formatted $ Kcal $ rnd k

    rnd :: Float -> Float
    rnd n = case v of
      Verbose -> n
      _       -> fromInteger $ round n
