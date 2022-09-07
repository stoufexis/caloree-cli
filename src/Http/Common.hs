module Http.Common
  ( reqSecure
  , reqUnsecure
  ) where

import           Control.Monad.RWS              ( MonadReader(ask), MonadIO  )
import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )
import           Model.Config                   ( AppConfig(..) )
import           Network.HTTP.Req
import qualified Network.HTTP.Req              as REQ
import Data.ByteString (ByteString)

makeReq
  :: ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     , MonadReader AppConfig m
     , MonadIO m
     , HttpMethod method
     , HttpBody body
     , FromJSON a
     )
  => (Text -> Url scheme)
  -> (ByteString -> ByteString -> Option scheme)
  -> method
  -> Text
  -> body
  -> Option scheme
  -> m (JsonResponse a)
makeReq fhttp fauth method path body params =
  ask >>= (\cnf -> runReq defaultHttpConfig $ req method (makeUri cnf) body jsonResponse (makeParams cnf))
 where
  makeUri (AppConfig { host }) = fhttp host /: path
  makeParams (AppConfig { username, password, port = p }) =
    fauth username password <> REQ.port p <> params


reqSecure
  :: ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     , MonadIO m
     , MonadReader AppConfig m
     , HttpMethod method
     , HttpBody body
     , FromJSON a
     )
  => method
  -> Text
  -> body
  -> Option 'Https
  -> m (JsonResponse a)
reqSecure = makeReq https basicAuth


reqUnsecure
  :: ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     , MonadIO m
     , MonadReader AppConfig m
     , HttpMethod method
     , HttpBody body
     , FromJSON a
     )
  => method
  -> Text
  -> body
  -> Option 'Http
  -> m (JsonResponse a)
reqUnsecure = makeReq http basicAuthUnsafe
