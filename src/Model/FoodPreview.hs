{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.FoodPreview
  ( FoodPreview
  ) where
import           Colonnade                      ( ascii
                                                , headed
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Vector                    ( indexed )
import           Fmt                            ( pretty )
import           GHC.Generics                   ( Generic )
import           Model.Types                    ( Description(..)
                                                , Id(Id)
                                                , Verbosity(..)
                                                )
import           Typeclass.Tabled

data FoodPreview = FoodPreview
  { id          :: Id FoodPreview
  , description :: Description
  }
  deriving (Show, Generic)

instance ToJSON FoodPreview
instance FromJSON FoodPreview

instance Tabled FoodPreview where
  table v = ascii (fromTable v) . indexed
   where
    fromTable Minimal =
      headed "id" $ show . (\(Id x) -> x) . Model.FoodPreview.id . snd
    fromTable _ = mconcat
      [ headed "#" (pretty . fst)
      , fromTable Minimal
      , headed "description" ((\(Description x) -> x) . description . snd)
      ]
