{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Http.LogRequest
  ( addLogRequest
  , getLogsRequest
  , removeLogRequest
  , undoLogRequest
  , updateLogRequest
  ) where
import           Control.Monad.RWS
import           Data.Aeson                     ( ToJSON )
import           Dto.AddLog                     ( AddLogDto(..) )
import           Dto.RemoveLog                  ( RemoveLogDto(..) )
import           Dto.UndoLog                    ( UndoLogDto(UndoLogDto) )
import           Dto.UpdateLog                  ( ModifyLogDto(ModifyLogDto) )
import           Http.Common                    ( reqUnsecure )
import           Model.Command                  ( LogFilters(..)
                                                , dateOrDefault
                                                )
import           Model.Config
import           Model.Log                      ( Log )
import           Model.Types                    ( Amount
                                                , EFID
                                                , PageLimit(..)
                                                )
import           Network.HTTP.Req
import           Typeclass.AsQueryParam         ( AsQueryParam(qparam) )
import           Typeclass.WithDefault          ( def )
import Model.DateTime

getLogsRequest
  :: (MonadReader AppConfig m, MonadIO m)
  => Maybe PageLimit
  -> LogFilters
  -> m [Log]
getLogsRequest pl fs = fmap responseBody request
 where
  request =
    dateOrDefault fs
      >>= reqUnsecure GET (/: "log") NoReqBody jsonResponse
      .   makeParams

  makeParams x =
    qparam x <> idParam fs <> intervalParam fs <> qparam (def pl) <> qparam
      (Offset 0)

  idParam LogFilters { fid = Just i } = qparam i
  idParam _                           = mempty

  intervalParam = qparam . def . interval

postLog
  :: (MonadReader AppConfig m, MonadIO m, ToJSON a) => a -> m IgnoreResponse
postLog b = reqUnsecure POST (/: "log") (ReqBodyJson b) ignoreResponse mempty

addLogRequest
  :: (MonadReader AppConfig m, MonadIO m)
  => Amount
  -> Date
  -> Time
  -> EFID
  -> m ()
addLogRequest amount date time fid = postLog body >> pure ()
  where body = AddLogDto fid amount (formatted date) (timeToMinutes time)

removeLogRequest :: (MonadReader AppConfig m, MonadIO m) => LogFilters -> m ()
removeLogRequest lf@LogFilters { fid, interval } =
  dateOrDefault lf >>= postLog . makeBody >> pure ()
  where makeBody d = RemoveLogDto fid (formatted d) (def interval)

undoLogRequest
  :: (MonadReader AppConfig m, MonadIO m) => LogFilters -> Int -> m ()
undoLogRequest lf@LogFilters { fid, interval } times =
  dateOrDefault lf >>= postLog . makeBody >> pure ()
  where makeBody d = UndoLogDto fid (formatted d) (def interval) times

updateLogRequest
  :: (MonadReader AppConfig m, MonadIO m) => LogFilters -> Amount -> m ()
updateLogRequest lf@LogFilters { fid, interval } amount =
  dateOrDefault lf >>= postLog . makeBody >> pure ()
  where makeBody d = ModifyLogDto fid amount (formatted d) (def interval)
