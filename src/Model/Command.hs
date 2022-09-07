{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Command
  ( Verbosity(..)
  , Command(..)
  ) where
import           Model.Nutrients
import           Model.Types
import Model.Types (Amount(Amount))

data LogFilters = LogFilters
  { grouping :: Maybe Minute
  , group    :: Maybe Integer
  , date     :: Maybe Date
  , fid      :: Maybe Id
  , offset   :: Maybe Offset
  }

data Command = AddLog (Amount, Date, Time, Integer)
             | UpdateLog (LogFilters, Amount)
             | DeleteLog LogFilters
             | UndoLog LogFilters
             | ViewLog (Maybe Verbosity, LogFilters, PageLimit)
             | SearchFood (Maybe Verbosity, Description, Maybe PageLimit )
             | ViewFood (Maybe Verbosity, Id, Maybe Amount)
             | SearchCustomFood (Maybe Verbosity, Description, Maybe PageLimit )
             | ViewCustomFood (Maybe Verbosity, Id, Maybe Amount )
             | AddCustomFood (Description, Nutrients)
             | DeleteCustomFood Id


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


