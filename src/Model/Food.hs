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
import qualified Data.Vector                   as V
import           Fmt                            ( pretty )
import           GHC.Generics                   ( Generic )
import           Model.Nutrients                ( Nutrients(..) )
import           Model.Types                    ( Amount(Amount)
                                                , Description(..)
                                                , Id(Id)
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
  table Minimal = pretty . (\(Id i) -> i) . Model.Food.id . V.head

  table Normal  = ascii fromTable . indexed
   where
    fromTable = mconcat
      [ headed "energy" (pretty . energy . nutrients . snd)
      , headed "#" $ pretty . fst
      , headed "id" $ pretty . (\(Id i) -> i) . Model.Food.id . snd
      , headed "description" $ pretty . trimmed Normal . description . snd
      , headed "amount" $ pretty . (\(Amount x) -> x) . grams . snd
      ]

  table Verbose = ascii fromTable . indexed
   where
    fromTable = mconcat
      [ headed "energy" (pretty . energy . nutrients . snd)
      , headed "#" $ pretty . fst
      , headed "id" $ pretty . (\(Id i) -> i) . Model.Food.id . snd
      , headed "description" $ pretty . trimmed Verbose . description . snd
      , headed "amount" $ pretty . (\(Amount x) -> x) . grams . snd
      , headed "protein" $ pretty . protein . nutrients . snd
      , headed "carbs" $ pretty . carbs . nutrients . snd
      , headed "fat" $ pretty . fat . nutrients . snd
      , headed "fiber" $ pretty . fiber . nutrients . snd
      ]

