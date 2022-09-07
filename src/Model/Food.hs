{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Food
  ( Food
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
                                                )
import           Typeclass.Tabled               ( Tabled(..) )

data Food = Food
  { id          :: Id Food
  , description :: Description
  , amount      :: Amount
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON Food
instance FromJSON Food

instance Tabled Food where
  table v = ascii (fromTable v) . indexed
   where
    fromTable Minimal = mconcat
      [ headed "#" $ pretty . fst
      , headed "description" $ (\(Description x) -> x) . description . snd
      , headed "amount" $ pretty . (\(Amount x) -> x) . amount . snd
      ]
    fromTable Normal =
      fromTable Minimal <> headed "energy" (pretty . energy . nutrients . snd)
    fromTable Verbose = mconcat
      [ fromTable Normal
      , headed "protein" $ pretty . protein . nutrients . snd
      , headed "carbs" $ pretty . carbs . nutrients . snd
      , headed "fat" $ pretty . fat . nutrients . snd
      , headed "fiber" $ pretty . fiber . nutrients . snd
      ]
