module Lib
  ( someFunc
  ) where
import           Colonnade                      ( ascii
                                                , headed
                                                )
import           Data.Text
import           Data.Vector                    ( Vector, indexed )
import           Fmt
import           Network.HTTP.Req               ( defaultHttpConfig
                                                , runReq
                                                )
someFunc :: IO ()
someFunc = putStr $ showMeals $ indexed meals

-- someFunc :: IO ()
-- someFunc = runReq defaultHttpConfig getFoodsByDescription >>= print

-- input :: IO ()
-- input = do
--   clearScreen
--   setCursorPosition 0 0
--   line <- getLine
--   putStrLn $ "You said: " <> line
--   someFunc

data MealFood = MealFood
  { mealName :: String
  , foodName :: String
  , amount   :: Int
  }

data Change a = None a | Added a | Updated a a | Deleted a deriving (Functor)

meals :: Vector (Change MealFood)
meals =
  [ Added MealFood { mealName = "Breakfast"
                   , foodName = "Bread, whole wheat"
                   , amount   = 125
                   }
  , None MealFood { mealName = "Lunch"
                  , foodName = "Protein Drink, Vanilla"
                  , amount   = 500
                  }
  , Deleted MealFood { mealName = "Breakfast"
                     , foodName = "Pork, Grounded, 93% lean"
                     , amount   = 200
                     }
  , Added MealFood { mealName = "Dinner"
                   , foodName = "Bread, whole wheat"
                   , amount   = 125
                   }
  , Added MealFood { mealName = "Dinner"
                   , foodName = "Pork, Grounded, 80% Lean"
                   , amount   = 200
                   }
  , Updated
    MealFood { mealName = "Dinner"
             , foodName = "Spinach, Baby, Raw"
             , amount   = 300
             }
    MealFood { mealName = "Dinner"
             , foodName = "Spinach, Baby, Raw"
             , amount   = 250
             }
  ]

showMeals :: Vector (Int, Change MealFood) -> String
showMeals = ascii formTable
 where
  formTable = mconcat
    [ headed ""       (show . fst)
    , headed "Meal"   (displayBefore . fmap mealName . snd)
    , headed "Food"   (displayBefore . fmap foodName . snd)
    , headed "Amount" (displayBefore . fmap amount . snd)
    , headed ""       (pretty . changeSymbol . snd)
    , headed "Meal"   (displayAfter . fmap mealName . snd)
    , headed "Food"   (displayAfter . fmap foodName . snd)
    , headed "Amount" (displayAfter . fmap amount . snd)
    ]

  displayBefore :: Buildable a => Change a -> String
  displayBefore = pretty . display before . fmap build

  displayAfter :: Buildable a => Change a -> String
  displayAfter = pretty . display after . fmap build

  display :: Buildable a => (Change Builder -> Maybe a) -> Change a -> Text
  display f x = pretty $ f $ fmap build x

  changeSymbol :: Change MealFood -> Builder
  changeSymbol (Added   _  ) = "++"
  changeSymbol (Deleted _  ) = "--"
  changeSymbol (None    _  ) = "~~"
  changeSymbol (Updated _ _) = "->"

  after (Added   x   ) = Just x
  after (Deleted _   ) = Nothing
  after (None    x   ) = Just x
  after (Updated _ x') = Just x'

  before (Added   _  ) = Nothing
  before (Deleted x  ) = Just x
  before (None    x  ) = Just x
  before (Updated x _) = Just x


