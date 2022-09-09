{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Food
  ( Food(..)
  ) where
import           Colonnade                      ( headed )
import           Control.Arrow                  ( Arrow(second) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Profunctor                ( Profunctor(lmap) )
import           Fmt                            ( pretty )
import           GHC.Generics                   ( Generic )
import           Model.DateTime                 ( formatted )
import           Model.Nutrients                ( Nutrients(..) )
import           Model.Types
import           Typeclass.Tabled               ( Tabled(..) )

data Food = Food
  { id          :: Id
  , description :: Description
  , grams       :: Amount
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON Food
instance FromJSON Food

instance Tabled Food where
  colonnade Minimal = mconcat
    [ headed "#" $ pretty . fst
    , headed "description" $ pretty . trimmed Normal . description . snd
    ]

  colonnade Normal = mconcat
    [ headed "energy" (pretty . energy . nutrients . snd)
    , headed "#" $ pretty . fst
    , headed "id" $ pretty . formatted . Model.Food.id . snd
    , headed "description" $ pretty . trimmed Normal . description . snd
    , headed "amount" $ pretty . formatted . grams . snd
    ]

  colonnade Verbose = mconcat
    [ headed "energy" (pretty . energy . nutrients . snd)
    , headed "#" $ pretty . fst
    , headed "id" $ pretty . formatted . Model.Food.id . snd
    , headed "description" $ pretty . trimmed Verbose . description . snd
    , headed "amount" $ pretty . formatted . grams . snd
    , ncol
    ]
    where ncol = lmap (second nutrients) $ colonnade Verbose

