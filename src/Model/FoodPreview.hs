{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.FoodPreview
  ( FoodPreview(..)
  ) where
import           Colonnade                      ( ascii
                                                , headed
                                                )
import           Data.Aeson
import qualified Data.Vector                   as V
import           Data.Vector                    ( indexed )
import           Fmt
import           GHC.Generics
import           Model.DateTime                 ( formatted )
import           Model.Types
import           Typeclass.Tabled

data FoodPreview = FoodPreview
  { id          :: Id
  , description :: Description
  }
  deriving (Show, Generic)

instance ToJSON FoodPreview
instance FromJSON FoodPreview

instance Tabled FoodPreview where
  colonnade Minimal = mconcat
    [ headed "#" (pretty . fst)
    , headed "id" $ pretty . formatted . Model.FoodPreview.id . snd
    ]

  colonnade Normal = mconcat
    [ headed "#" (pretty . fst)
    , headed "id" $ pretty . formatted . Model.FoodPreview.id . snd
    , headed "description" (pretty . trimmed Normal . description . snd)
    ]

  colonnade Verbose = mconcat
    [ headed "#" (pretty . fst)
    , headed "id" $ pretty . formatted . Model.FoodPreview.id . snd
    , headed "description" (pretty . trimmed Verbose . description . snd)
    ]

  table Minimal = pretty . formatted . Model.FoodPreview.id . V.head
  table v       = pretty . ascii (colonnade v) . indexed

