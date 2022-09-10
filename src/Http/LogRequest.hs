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
import           Fmt                            ( pretty )
import           Http.Common                    ( reqUnsecure )
import           Model.Command                  ( LogFilters(..)
                                                , dateOrDefault
                                                , timeOrDefault
                                                )
import           Model.Config
import           Model.DateTime
import           Model.Log                      ( Log )
import           Model.Types
import           Network.HTTP.Req
import           Typeclass.AsQueryParam         ( AsQueryParam(qparam) )
import           Typeclass.WithDefault          ( def )

getLogsRequest
  :: (MonadReader AppConfig m, MonadIO m)
  => Maybe Page
  -> Maybe Limit
  -> LogFilters
  -> m [Log]
getLogsRequest p l fs@LogFilters { date } = fmap responseBody request
 where
  request =
    dateOrDefault date
      >>= reqUnsecure GET (/: "log") NoReqBody jsonResponse
      .   makeParams

  makeParams x =
    qparam x
      <> idParam fs
      <> intervalParam fs
      <> qparam (def p)
      <> qparam (def l)
      <> qparam (Offset 0)

  idParam LogFilters { fid = Just i } = qparam i
  idParam _                           = mempty

  intervalParam = qparam . def . interval

postLog :: (MonadReader AppConfig m, MonadIO m, ToJSON a) => a -> m ()
postLog b =
  reqUnsecure POST (/: "log") (ReqBodyJson b) ignoreResponse mempty >> pure ()

addLogRequest
  :: forall m
   . (MonadReader AppConfig m, MonadIO m)
  => Amount
  -> Maybe Date
  -> Maybe Time
  -> EFID
  -> m ()
addLogRequest amount date time fid = body >>= postLog
 where
  body =
    AddLogDto fid amount
      <$> fmap (pretty . formatted) (dateOrDefault date)
      <*> fmap timeToMinutes        (timeOrDefault time)

removeLogRequest :: (MonadReader AppConfig m, MonadIO m) => LogFilters -> m ()
removeLogRequest LogFilters { date, fid, interval } =
  dateOrDefault date >>= postLog . makeBody
  where makeBody d = RemoveLogDto fid (pretty $ formatted d) (def interval)

undoLogRequest
  :: (MonadReader AppConfig m, MonadIO m) => LogFilters -> Int -> m ()
undoLogRequest LogFilters { date, fid, interval } times =
  dateOrDefault date >>= postLog . makeBody
  where makeBody d = UndoLogDto fid (pretty $ formatted d) (def interval) times

updateLogRequest
  :: (MonadReader AppConfig m, MonadIO m) => LogFilters -> Amount -> m ()
updateLogRequest LogFilters { date, fid, interval } amount =
  dateOrDefault date >>= postLog . makeBody
 where
  makeBody d = ModifyLogDto fid amount (pretty $ formatted d) (def interval)
