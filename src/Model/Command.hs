{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Command
  ( Verbosity
  , Command
  ) where
import           Model.CustomFood
import           Model.Food
import           Model.Nutrients
import           Model.Types

data LogFilters = LogFilters
  { grouping :: Minute
  , group    :: Integer
  , date     :: Date
  , fid      :: Either (Id Food) (Id CustomFood)
  , offset   :: Offset
  }

data FoodSearchParams = FoodSearchParams
  { foodSearchVerbosity   :: Verbosity
  , foodSearchDescription :: Description
  , foodSearchPageLimit   :: PageLimit
  }

data FoodViewParams = FoodViewParams
  { foodViewVerbosity :: Verbosity
  , foodViewId        :: Id Food
  , foodViewAmount    :: Amount
  }

data Command = AddLog (Amount, Date, Time, Integer)
             | UpdateLog (LogFilters, Amount)
             | DeleteLog LogFilters
             | UndoLog LogFilters
             | ViewLog (Verbosity, LogFilters, PageLimit)
             | SearchFood FoodSearchParams
             | ViewFood FoodViewParams
             | AddCustomFood (Description, Nutrients)
             | DeleteFood Integer
             | SearchCustomFood FoodSearchParams
             | ViewCustomFood FoodViewParams

--  log
--    - add    --amount <grams> --day <date> --time <time>  <food_id>
--    - update --grouping <minutes> --day <date> --group <int> --fid <food_id> --amount <int> 
--    - delete --grouping <minutes> --day <date> --group <int> --fid <food_id> 
--    - undo   --grouping <minutes> --day <date> --group <int> --fid <food_id> 
--    - view   --verbosity 0-2 --grouping <minutes> --day <date> --group <int> --fid <food_id> --page <int> --limit <int>
--  food 
--    - search --verbosity 0-2 --description <string> --page <int> --limit <int>
--    - view   --verbosity 0-2 --fid <food_id> --amount <grams>
--  cf 
--    - add    --description <string> --energy <kcal> --protein <grams> --carbs <grams> --fat <grams> --fiber <grams>
--    - delete --cfid <custom_food_id> 
--    - search --verbosity 0-2 --description <string> --page <int> --limit <int>
--    - view   --verbosity 0-2 --cfid <custom_food_id> --amount <grams>
--


