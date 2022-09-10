{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Command
  ( Verbosity(..)
  , Command(..)
  , LogFilters(..)
  , dateOrDefault
  ) where
import           Control.Monad.RWS              ( MonadReader(ask) )
import           Model.Config                   ( AppConfig(..) )
import           Model.DateTime
import           Model.Nutrients
import           Model.Types

data LogFilters = LogFilters
  { interval :: Maybe Inteval
  , date     :: Maybe Date
  , fid      :: Maybe EFID
  }

dateOrDefault :: MonadReader AppConfig m => LogFilters -> m Date
dateOrDefault (LogFilters { date = Just d }) = pure d
dateOrDefault _ = fmap (\(AppConfig { date = d }) -> d) ask

data Command = AddLog Amount (Maybe Date) (Maybe Time) EFID
             | UpdateLog LogFilters Amount
             | RemoveLog LogFilters
             | UndoLog LogFilters Int
             | ViewLog (Maybe Verbosity) LogFilters (Maybe Page) (Maybe Limit)
             | SearchFood (Maybe Verbosity) Description (Maybe Page) (Maybe Limit)
             | ViewFood (Maybe Verbosity) Id (Maybe Amount)
             | SearchCustomFood (Maybe Verbosity) Description (Maybe Page) (Maybe Limit)
             | ViewCustomFood (Maybe Verbosity) Id (Maybe Amount)
             | AddCustomFood Description Nutrients
             | DeleteCustomFood Id

deriving instance Show LogFilters
deriving instance Show Command

--  log
--    - add    --amount <grams> --day <date> --time <time>  <food_id>
--    - update --window <minutes> --offset <offset> --day <date> --group <int> --fid <food_id> --cfid <custom_food_id> --amount <int> 
--    - delete --window <minutes> --offset <offset> --day <date> --group <int> --fid <food_id> --cfid <custom_food_id>
--    - undo   --window <minutes> --offset <offset> --day <date> --group <int> --fid <food_id> --cfid <custom_food_id> --times <int>
--    - view   --verbosity 0-2 --window <minutes> --day <date> --group <int> --fid <food_id> --cfid <custom_food_id> --page <int> --limit <int>
--  food 
--    - search --verbosity 0-2 --description <string> --page <int> --limit <int>
--    - view   --verbosity 0-2 --fid <food_id> --amount <grams>
--  cf 
--    - add    --description <string> --energy <kcal> --protein <grams> --carbs <grams> --fat <grams> --fiber <grams>
--    - delete --cfid <custom_food_id> 
--    - search --verbosity 0-2 --description <string> --page <int> --limit <int>
--    - view   --verbosity 0-2 --cfid <custom_food_id> --amount <grams>
--


