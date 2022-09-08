module Model.CustomFoodPreview
  ( CustomFoodPreview
  ) where
import           Colonnade
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Vector                   as V
import           Data.Vector
import           Fmt
import           GHC.Generics                   ( Generic )
import           Model.Types                    ( Description
                                                , Id(Id)
                                                , Verbosity(..)
                                                , trimmed
                                                )
import           Typeclass.Tabled               ( Tabled(..) )


data CustomFoodPreview = CustomFoodPreview
  { id          :: Id
  , userId      :: Id
  , description :: Description
  }
  deriving (Show, Generic)

instance FromJSON CustomFoodPreview
instance ToJSON CustomFoodPreview

instance Tabled CustomFoodPreview where
  colonnade Minimal = mconcat
    [ headed "#" (pretty . fst)
    , headed "id" $ pretty . (\(Id x) -> x) . Model.CustomFoodPreview.id . snd
    ]

  colonnade Normal = mconcat
    [ headed "#" (pretty . fst)
    , headed "id" $ pretty . (\(Id x) -> x) . Model.CustomFoodPreview.id . snd
    , headed "description" (pretty . trimmed Normal . description . snd)
    ]

  colonnade Verbose = mconcat
    [ headed "#" (pretty . fst)
    , headed "id" $ pretty . (\(Id x) -> x) . Model.CustomFoodPreview.id . snd
    , headed "description" (pretty . trimmed Verbose . description . snd)
    ]

  table Minimal = pretty . (\(Id x) -> x) . Model.CustomFoodPreview.id . V.head
  table v       = ascii (colonnade v) . indexed

