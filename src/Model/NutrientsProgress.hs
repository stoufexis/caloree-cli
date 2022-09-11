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
import           Typeclass.Tabled

data NutrientsProgress = NutrientsProgress
  { nutrientName :: Text
  , progress     :: Float
  , target       :: Float
  }

makeProgress :: Nutrients -> Nutrients -> [NutrientsProgress]
makeProgress nutrients nutrients' =
  [ mk energy  "energy"
  , mk protein "protein"
  , mk carbs   "carbs"
  , mk fat     "fat"
  , mk fiber   "fiber"
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
      build progress |+ " - " +| build target

    ratio _ NutrientsProgress { progress, target } =
      build (round progress :: Integer) |+ " - " +| build (round target :: Integer)

    progressBar NutrientsProgress { progress = p, target = t } =
      T.unfoldr (unfoldf (p * 100 / t)) 0

    unfoldf p x | x >= 100  = Nothing
                | x >= p    = Just ('-', x + 1)
                | otherwise = Just ('#', x + 1)


