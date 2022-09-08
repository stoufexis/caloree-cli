{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.FoodPreview
  ( FoodPreview(..)
  ) where
import           Colonnade                      ( ascii
                                                , headed
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Vector                    ( indexed )
import qualified Data.Vector                   as V
import           Fmt                            ( pretty )
import           GHC.Generics                   ( Generic )
import           Model.Types                    ( Description(..)
                                                , Id(Id)
                                                , Verbosity(..)
                                                , trimmed
                                                )
import           Typeclass.Tabled

data FoodPreview = FoodPreview
  { id          :: Id
  , description :: Description
  }
  deriving (Show, Generic)

instance ToJSON FoodPreview
instance FromJSON FoodPreview

instance Tabled FoodPreview where
  table Minimal = pretty . (\(Id x) -> x) . Model.FoodPreview.id . V.head
  table Normal  = ascii fromTable . indexed
   where
    fromTable = mconcat
      [ headed "#" (pretty . fst)
      , headed "id" $ pretty . (\(Id x) -> x) . Model.FoodPreview.id . snd
      , headed "description" (pretty . trimmed Normal . description . snd)
      ]

  table Verbose = ascii fromTable . indexed
   where
    fromTable = mconcat
      [ headed "#" (pretty . fst)
      , headed "id" $ pretty . (\(Id x) -> x) . Model.FoodPreview.id . snd
      , headed "description" (pretty . trimmed Verbose . description . snd)
      ]
