module Model.Log
  ( Log
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
                                                , minutesToTime
                                                )

import           Model.Nutrients
import           Model.Types
import           Typeclass.Formatted
import           Typeclass.Tabled               ( Tabled(..) )

data Log = Log
  { id          :: EFID
  , day         :: Text
  , minute      :: Minute
  , description :: Description
  , amount      :: Amount
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON Log
instance FromJSON Log

instance Tabled Log where
  colonnade v = mconcat
    [ headed "#" $ pretty . fst
    , headed "id" $ pretty . formatted . Model.Log.id . snd
    , headed "description" $ pretty . trimmed v . description . snd
    , headed "time" $ pretty . formatted . minutesToTime . minute . snd
    , headed "amount" $ pretty . formatted . amount . snd
    , case v of
      Verbose -> ncol
      Normal  -> ncol
      Minimal -> mempty
    ]
    where ncol = lmap (second nutrients) $ colonnade v

