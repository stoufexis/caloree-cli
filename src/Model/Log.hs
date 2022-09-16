module Model.Log
  ( Log(..)
  , sumNutrients
  , roundTime
  ) where
import           Colonnade
import           Control.Arrow                  ( Arrow(second) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Profunctor                ( Profunctor(lmap) )
import           Data.Text                      ( Text )
import           Fmt
import           GHC.Generics                   ( Generic )
import           Model.DateTime                 ( Minute(..)
                                                , TimeRound(TimeRound)
                                                , minutesToTime
                                                )

import           Data.Vector                    ( indexed )
import qualified Data.Vector                   as V
import           Model.Nutrients
import           Model.Types
import           RenderTable                    ( renderTable )
import           Typeclass.Formatted
import           Typeclass.Tabled               ( Tabled(..) )

data Log = Log
  { id          :: EFID
  , day         :: Text
  , minute      :: Minute
  , description :: Description
  , amount      :: Grams
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

sumNutrients :: [Log] -> Nutrients
sumNutrients = mconcat . map nutrients

roundTime :: TimeRound -> [Log] -> [Log]
roundTime (TimeRound m) = map makeTime
 where
  makeTime l@Log { minute = Minute m' } =
    let rest       = m' `mod` m
        additional = if rest > (m `div` 2) then m else 0
    in  l { minute = Minute $ m' `div` m * m + additional }

instance ToJSON Log
instance FromJSON Log

instance Tabled Log where
  colonnade v = mconcat
    [ headed "#" $ pretty . fst
    , headed "time" $ pretty . formatted . minutesToTime . minute . snd
    , headed "id" $ pretty . formatted . Model.Log.id . snd
    , headed "description" $ pretty . trimmed v . description . snd
    , headed "amount" $ pretty . formatted . amount . snd
    , case v of
      Verbose -> ncol
      Normal  -> ncol
      Minimal -> mempty
    ]
    where ncol = lmap (second nutrients) $ colonnade v

  table Minimal = pretty . formatted . Model.Log.id . V.head
  table v       = renderTable (colonnade v) . indexed

