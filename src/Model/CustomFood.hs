{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Model.CustomFood
  ( CustomFood(..)
  ) where
import           Colonnade
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Fmt
import           GHC.Generics                   ( Generic )
import           Model.Nutrients                ( Nutrients(..) )
import           Model.Types
import           Typeclass.Tabled               ( Tabled(..) )

data CustomFood = CustomFood
  { id          :: Id
  , userId      :: Id
  , description :: Description
  , grams       :: Amount
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON CustomFood
instance FromJSON CustomFood

instance Tabled CustomFood where
  colonnade Minimal = mconcat
    [ headed "#" $ pretty . fst
    , headed "description" $ pretty . trimmed Normal . description . snd
    ]

  colonnade Normal = mconcat
    [ headed "energy" (pretty . energy . nutrients . snd)
    , headed "#" $ pretty . fst
    , headed "id" $ pretty . (\(Id i) -> i) . Model.CustomFood.id . snd
    , headed "description" $ pretty . trimmed Normal . description . snd
    , headed "amount" $ pretty . (\(Amount x) -> x) . grams . snd
    ]

  colonnade Verbose = mconcat
    [ headed "energy" (pretty . energy . nutrients . snd)
    , headed "#" $ pretty . fst
    , headed "id" $ pretty . (\(Id i) -> i) . Model.CustomFood.id . snd
    , headed "description" $ pretty . trimmed Verbose . description . snd
    , headed "amount" $ pretty . (\(Amount x) -> x) . grams . snd
    , headed "protein" $ pretty . protein . nutrients . snd
    , headed "carbs" $ pretty . carbs . nutrients . snd
    , headed "fat" $ pretty . fat . nutrients . snd
    , headed "fiber" $ pretty . fiber . nutrients . snd
    ]
