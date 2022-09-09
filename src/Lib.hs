{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib
  () where
import           Control.Monad.Reader
import qualified Data.Vector                   as V
import           Http.CustomFoodRequest         ( addCustomFood
                                                , deleteCustomFood
                                                , getCustomFood
                                                , getCustomFoods
                                                )
import           Http.FoodRequest               ( getFood
                                                , getFoods
                                                )
import           Http.LogRequest                ( addLogRequest
                                                , getLogsRequest
                                                , removeLogRequest
                                                , undoLogRequest
                                                , updateLogRequest
                                                )
import           Model.Command                  ( Command(..) )
import           Model.Config                   ( AppConfig(..) )
import           Model.Types
import           Typeclass.Tabled
import           Typeclass.WithDefault          ( def )

execute :: (Functor m, Tabled a) => m [a] -> Verbosity -> m String
execute req v = fmap (table v . V.fromList) req

execute_ :: (Monad m) => m () -> m String
execute_ req = req >> pure "Ok!"

type AppIO = ReaderT AppConfig IO

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

-- run :: IO ()
-- run = runReaderT (exec >>= liftIO . putStrLn) cnf
--  where
--   exec = executeCommand
--     (ViewLog
--        Just Normal
--        LogFilters { interval = Nothing
--                    , date     = Nothing
--                    , fid      = Nothing
--                    , cfid     = Nothing
--                    }
--        Nothing

--     )
--   cnf = AppConfig { host     = "localhost"
--                   , port     = 8080
--                   , username = "stef1"
--                   , password = "password1"
--                   , date     = Date { year = 2022, month = 9, day = 8 }
--                   }
