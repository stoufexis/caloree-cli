{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Tmp
  () where
import           Control.Monad.Cont
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT) )
import qualified Data.Vector                   as V
import           Model.Config                   ( AppConfig(..) )
import           Model.Types                    ( Description(Description)
                                                , PageLimit(..), Verbosity(..)
                                                )
import           Network.HTTP.Req
import           Typeclass.Requested            ( Requested(request) )
import           Typeclass.Tabled               ( Tabled(table) )




execute :: (Functor m, Requested m p a, Tabled a) => Verbosity -> p -> m String
execute v p = fmap (table v . V.fromList) $ request p


type AppIO = ReaderT AppConfig IO

runApp :: AppIO a -> AppConfig -> IO a
runApp app cnf = runReq defaultHttpConfig $ liftIO $ runReaderT app cnf

run :: Verbosity -> String -> IO ()
run v s = app cnf >>= putStrLn
 where
  cnf = AppConfig { host     = "localhost"
                  , port     = 8080
                  , username = "stef1"
                  , password = "password1"
                  }
  app = runApp
    (execute v (Description s, PageLimit { page = 0, limit = 10 }))

