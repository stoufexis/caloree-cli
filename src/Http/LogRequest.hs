{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Http.LogRequest
  ( addLogRequest
  , getLogsRequest
  ) where
import           Control.Monad.RWS
import           Data.Text                      ( Text )
import           Dto.AddLog                     ( AddLogDto(..)
                                                , FID(..)
                                                )
import           Fmt
import           Http.Common                    ( reqUnsecure )
import           Model.Command                  ( LogFilters(..) )
import           Model.Config
import           Model.Log                      ( Log )
import           Model.Types                    ( Amount
                                                , Date(..)
                                                , Id(Id)
                                                , Minute(Minute)
                                                , Offset(Offset)
                                                , PageLimit(..)
                                                , Time(..)
                                                )
import           Network.HTTP.Req
import           Typeclass.AsQueryParam         ( AsQueryParam(qparam) )
import           Typeclass.WithDefault          ( def )

getLogsRequest
  :: (MonadReader AppConfig m, MonadIO m) => (PageLimit, LogFilters) -> m [Log]
getLogsRequest (pl, fs) = fmap responseBody request
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
  :: (MonadReader AppConfig m, MonadIO m) => (Amount, Date, Time, Id) -> m ()
addLogRequest (a, Date { year, month, day }, Time { hour, minute }, i) =
  request >> pure ()
 where
  m :: Text
  m = if month < 10 then "0" +| month |+ "" else pretty month
  d :: Text
  d    = if day < 10 then "0" +| day |+ "" else pretty day

  body = ReqBodyJson AddLogDto
    { t      = "Add"
    , fid    = FID { food_id = i }
    , amount = a
    , day    = "" +| year |+ "-" +| m |+ "-" +| d |+ ""
    , minute = Minute (hour * 60 + minute)
    }

  request = reqUnsecure POST (/: "log") body ignoreResponse mempty

