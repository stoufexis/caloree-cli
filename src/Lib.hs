{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Lib
  ( executeCommand
  ) where
import           Control.Monad.Reader
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Http.CustomFoodRequest
import           Http.FoodRequest
import           Http.LogRequest
import           Http.User                      ( getTargetNutrients )
import           Model.Command                  ( Command(..)
                                                , LogFilters(..)
                                                )
import           Model.Config                   ( AppConfig(..) )
import           Model.DateTime                 ( Inteval(Inteval), Minute (Minute) )
import           Model.Log
import           Model.NutrientsProgress
import           Model.Types
import           Typeclass.Tabled
import           Typeclass.WithDefault          ( def )

execute :: (Functor m, Tabled a) => m [a] -> Verbosity -> m Text
execute req v = fmap (table v . V.fromList) req

execute_ :: (Monad m) => m () -> m Text
execute_ req = req >> pure "Ok!"

executeCommand :: (MonadReader AppConfig m, MonadIO m) => Command -> m Text
executeCommand (SearchFood v d p l) = execute (getFoods d p l) $ def v
executeCommand (SearchCustomFood v d p l) =
  execute (getCustomFoods d p l) $ def v

executeCommand (ViewFood       v i a) = execute (getFood i a) $ def v
executeCommand (ViewCustomFood v i a) = execute (getCustomFood i a) $ def v

executeCommand (ViewLog (Just Minimal) f p l) =
  execute (getLogsRequest p l f) Minimal

executeCommand (ViewLog v f p l) =
  (\logs targets ->
      showLogsAndProgress logs (makeProgress (sumNutrients logs) targets)
    )
    <$> fmap (logsRoundTime (interval f)) (getLogsRequest p l f)
    <*> getTargetNutrients
 where
  logsRoundTime (Inteval (Just m) _ _) = roundTime m
  logsRoundTime _                      = roundTime $ Minute 15

  showLogsAndProgress logs progresses = T.unlines
    [table (def v) (V.fromList logs), table (def v) (V.fromList progresses)]




executeCommand (UpdateLog lf a    ) = execute_ $ updateLogRequest lf a
executeCommand (UndoLog   lf t    ) = execute_ $ undoLogRequest lf t
executeCommand (AddLog a d t i    ) = execute_ $ addLogRequest a d t i
executeCommand (RemoveLog lf      ) = execute_ $ removeLogRequest lf
executeCommand (AddCustomFood d n ) = execute_ $ addCustomFood d n
executeCommand (DeleteCustomFood i) = execute_ $ deleteCustomFood i


