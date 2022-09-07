{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Food
  ( Food(..)
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
import           Model.Nutrients                ( Nutrients(..) )
import           Model.Types                    ( Amount(Amount)
                                                , Description(..)
                                                , Id
                                                , Verbosity
                                                  ( Minimal
                                                  , Normal
                                                  , Verbose
                                                  )
                                                , trimmed
                                                )
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
  table v = ascii (fromTable v) . indexed
   where
    fromTableBase v' = mconcat
      [ headed "#" $ pretty . fst
      , headed "description" $ pretty . trimmed v' . description . snd
      , headed "amount" $ pretty . (\(Amount x) -> x) . grams . snd
      ]

    fromTableExtra Minimal = mempty
    fromTableExtra Normal  = headed "energy" (pretty . energy . nutrients . snd)
    fromTableExtra Verbose = mconcat
      [ fromTableExtra Normal
      , headed "protein" $ pretty . protein . nutrients . snd
      , headed "carbs" $ pretty . carbs . nutrients . snd
      , headed "fat" $ pretty . fat . nutrients . snd
      , headed "fiber" $ pretty . fiber . nutrients . snd
      ]

    fromTable v' = fromTableBase v' <> fromTableExtra v'