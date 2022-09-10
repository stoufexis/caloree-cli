{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Lib
  ( executeCommand
  ) where
import           Control.Monad.Reader
import qualified Data.Vector                   as V
import           Http.CustomFoodRequest
import           Http.FoodRequest
import           Http.LogRequest
import           Model.Command                  ( Command(..) )
import           Model.Config                   ( AppConfig(..) )
import           Model.Types
import           Typeclass.Tabled
import           Typeclass.WithDefault          ( def )

execute :: (Functor m, Tabled a) => m [a] -> Verbosity -> m String
execute req v = fmap (table v . V.fromList) req

execute_ :: (Monad m) => m () -> m String
execute_ req = req >> pure "Ok!"

executeCommand :: (MonadReader AppConfig m, MonadIO m) => Command -> m String
executeCommand (SearchFood v d p l) = execute (getFoods d p l) $ def v
executeCommand (SearchCustomFood v d p l) =
  execute (getCustomFoods d p l) $ def v

executeCommand (ViewFood       v i a) = execute (getFood i a) $ def v
executeCommand (ViewCustomFood v i a) = execute (getCustomFood i a) $ def v
executeCommand (ViewLog v f p l     ) = execute (getLogsRequest p l f) $ def v

executeCommand (UpdateLog lf a      ) = execute_ $ updateLogRequest lf a
executeCommand (UndoLog   lf t      ) = execute_ $ undoLogRequest lf t
executeCommand (AddLog a d t i      ) = execute_ $ addLogRequest a d t i
executeCommand (RemoveLog lf        ) = execute_ $ removeLogRequest lf
executeCommand (AddCustomFood d n   ) = execute_ $ addCustomFood d n
executeCommand (DeleteCustomFood i  ) = execute_ $ deleteCustomFood i


