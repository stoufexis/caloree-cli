module Model.NutrientsProgress
  ( NutrientsProgress(..)
  , makeProgress
  ) where
import           Colonnade                      ( headed )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Fmt
import           Model.Nutrients                ( Nutrients(..) )
import           Model.Types
import           Typeclass.Formatted
import           Typeclass.Tabled
import Data.Vector

data ProgressUnit = G Grams | E Kcal

toFloat :: ProgressUnit -> Float
toFloat (G (Grams g)) = g
toFloat (E (Kcal  g)) = g

roundPU :: ProgressUnit -> ProgressUnit
roundPU (G (Grams g)) = G $ Grams $ fromInteger $ round g
roundPU (E (Kcal  g)) = E $ Kcal $ fromInteger $ round g

instance Formatted ProgressUnit where
  formatted (G g) = formatted g
  formatted (E g) = formatted g

data NutrientsProgress = NutrientsProgress
  { nutrientName :: Text
  , progress     :: ProgressUnit
  , target       :: ProgressUnit
  }

makeProgress :: Nutrients -> Nutrients -> Vector NutrientsProgress
makeProgress nutrients nutrients' =
  [ mk (E . energy)  "energy"
  , mk (G . protein) "protein"
  , mk (G . carbs)   "carbs"
  , mk (G . fat)     "fat"
  , mk (G . fiber)   "fiber"
  ]
 where
  mk pick name = NutrientsProgress name (pick nutrients) (pick nutrients')

instance Tabled NutrientsProgress where
  colonnade v = mconcat
    [ headed "#" $ pretty . fst
    , headed "nutrient" $ pretty . nutrientName . snd
    , headed "ratio" $ pretty . ratio v . snd
    , headed "progress" $ pretty . progressBar . snd
    ]
   where
    ratio :: Verbosity -> NutrientsProgress -> Builder
    ratio Verbose NutrientsProgress { progress, target } =
      formatted progress |+ " - " +| formatted target

    ratio _ NutrientsProgress { progress, target } =
      formatted (roundPU progress) |+ " - " +| formatted (roundPU target)

    progressBar NutrientsProgress { progress, target } =
      let p = toFloat progress
          t = toFloat target
      in  T.unfoldr (unfoldf (p * 100 / t)) 0

    unfoldf p x | x >= 100  = Nothing
                | x >= p    = Just ('-', x + 1)
                | otherwise = Just ('#', x + 1)


