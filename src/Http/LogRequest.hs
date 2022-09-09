{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Http.LogRequest
  ( addLogRequest
  , getLogsRequest
  ) where
import           Control.Monad.RWS
import           Data.Text                      ( Text )
import           Dto.AddLog                     ( AddLogDto(..) )
import           Fmt
import           Http.Common                    ( reqUnsecure )
import           Model.Command                  ( LogFilters(..) )
import           Model.Config
import           Model.Log                      ( Log )
import           Model.Types                    ( Amount
                                                , Date(..)
                                                , EFID
                                                , Id(Id)
                                                , Minute(Minute)
                                                , Offset(Offset)
                                                , PageLimit(..)
                                                , Time(..)
                                                , formatted
                                                , timeToMinutes
                                                )
import           Network.HTTP.Req
import           Typeclass.AsQueryParam         ( AsQueryParam(qparam) )
import           Typeclass.WithDefault          ( def )

getLogsRequest
  :: (MonadReader AppConfig m, MonadIO m) => PageLimit -> LogFilters -> m [Log]
getLogsRequest pl fs = fmap responseBody request
 where
  request = params >>= reqUnsecure GET path NoReqBody jsonResponse

  path    = (/: "log")

  params  = fmap
    (\x -> x <> idParam fs <> filtersParam fs <> qparam pl <> qparam (Offset 0))
    (dateParam fs)

  dateParam (LogFilters { date = Just d }) = pure $ qparam d
  dateParam _ = fmap (\(AppConfig { date = d }) -> qparam d) ask

  idParam LogFilters { fid = Just (Id i) } = "food_id" =: i
  idParam LogFilters { cfid = Just (Id i) } = "custom_food_id" =: i
  idParam _ = mempty

  filtersParam (LogFilters { interval }) = qparam $ def interval

addLogRequest
  :: (MonadReader AppConfig m, MonadIO m)
  => Amount
  -> Date
  -> Time
  -> EFID
  -> m ()
addLogRequest a d t i = request >> pure ()
 where
  body = ReqBodyJson AddLogDto { fid    = i
                               , amount = a
                               , day    = formatted d
                               , minute = timeToMinutes t
                               }

  request = reqUnsecure POST (/: "log") body ignoreResponse mempty

