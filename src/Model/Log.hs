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
import           Fmt                            ( pretty )
import           GHC.Generics                   ( Generic )
import           Model.Nutrients
import           Model.Types                    ( Description
                                                , Id(Id)
                                                , Minute(Minute)
                                                , trimmed
                                                )
import           Typeclass.Tabled               ( Tabled(..) )

data Log = Log
  { id          :: Id
  , day         :: Text
  , minute      :: Minute
  , description :: Description
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON Log
instance FromJSON Log

instance Tabled Log where
  colonnade v = mconcat
    [ headed "#" $ pretty . fst
    , headed "id" $ pretty . (\(Id i) -> i) . Model.Log.id . snd
    , headed "at" $ pretty . (\(Minute m) -> m) . minute . snd
    , headed "description" $ pretty . trimmed v . description . snd
    , ncol
    ]
    where ncol = lmap (second nutrients) $ colonnade v
