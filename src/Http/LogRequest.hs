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
      <> qparam (def $ interval fs)
      <> qparam (def p)
      <> qparam (def l)
      <> qparam (Offset 0)

  idParam LogFilters { fid = Just i } = qparam i
  idParam _                           = mempty

postLog :: (MonadReader AppConfig m, MonadIO m, ToJSON a) => a -> m ()
postLog b =
  reqUnsecure POST (/: "log") (ReqBodyJson b) ignoreResponse mempty >> pure ()

addLogRequest
  :: forall m
   . (MonadReader AppConfig m, MonadIO m)
  => Grams
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

removeLogRequest
  :: (MonadReader AppConfig m, MonadIO m) => Maybe Date -> EntryNum -> m ()
removeLogRequest date (EntryNum num) =
  dateOrDefault date >>= \d -> postLog $ RemoveLogDto (pretty $ formatted d) num

undoLogRequest
  :: (MonadReader AppConfig m, MonadIO m) => Maybe Date -> Maybe UndoTimes -> m ()
undoLogRequest date times =
  dateOrDefault date >>= \d -> postLog $ UndoLogDto (pretty $ formatted d) (def times)

updateLogRequest
  :: (MonadReader AppConfig m, MonadIO m)
  => Grams
  -> Maybe Date
  -> EntryNum
  -> m ()
updateLogRequest amount date (EntryNum num) = dateOrDefault date
  >>= \d -> postLog $ ModifyLogDto amount (pretty $ formatted d) num
