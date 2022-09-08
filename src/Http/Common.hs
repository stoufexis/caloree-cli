{-# OPTIONS_GHC -Wno-unused-matches #-}
module Http.Common
  ( reqUnsecure
  ) where
import           Control.Monad.RWS              ( MonadIO
                                                , MonadReader(ask)
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Data                      ( Proxy )
import           Data.Text                      ( Text )
import           Model.Config                   ( AppConfig(..) )
import           Network.HTTP.Req
import qualified Network.HTTP.Req              as REQ

makeReq
  :: ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     , MonadIO m
     , HttpMethod method
     , HttpBody body
     , HttpResponse b
     , MonadReader AppConfig m
     )
  => (Text -> t)
  -> (ByteString -> ByteString -> Option scheme)
  -> method
  -> (t -> Url scheme)
  -> body
  -> Proxy b
  -> Option scheme
  -> m b
makeReq fhttp fauth method fpath body response params = ask >>= run
 where
  run cnf = runReq defaultHttpConfig
    $ req method (makeUri cnf) body response (makeParams cnf)

  makeUri (AppConfig { host }) = fpath $ fhttp host

  makeParams (AppConfig { username, password, port = p }) =
    fauth username password <> REQ.port p <> params


reqUnsecure
  :: ( HttpBodyAllowed (AllowsBody p1) (ProvidesBody p2)
     , MonadReader AppConfig m
     , MonadIO m
     , HttpMethod p1
     , HttpBody p2
     , HttpResponse b
     )
  => p1
  -> (Url 'Http -> Url scheme)
  -> p2
  -> Proxy b
  -> Option scheme
  -> m b
reqUnsecure = makeReq http basicAuthUnsafe
