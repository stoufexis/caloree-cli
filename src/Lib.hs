{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib
  ( someFunc
  ) where
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
import           Model.Command                  ( Command(..) )
import           Model.Config                   ( AppConfig(..) )
import           Model.Types
import           Typeclass.Tabled
import           Typeclass.WithDefault          ( def )

someFunc :: IO ()
someFunc = putStrLn "Hello World!"

execute :: (Functor m, Tabled a) => m [a] -> Verbosity -> m String
execute req v = fmap (table v . V.fromList) req

execute_ :: (Monad m) => m () -> m String
execute_ req = req >> pure "Ok!"

type AppIO = ReaderT AppConfig IO

executeCommand :: (MonadReader AppConfig m, MonadIO m) => Command -> m String
executeCommand = \case
  SearchFood       (v, d, pl) -> execute (getFoods (d, def pl)) (def v)
  ViewFood         (v, i, a ) -> execute (getFood (i, def a)) (def v)
  SearchCustomFood (v, d, pl) -> execute (getCustomFoods (d, def pl)) (def v)
  ViewCustomFood   (v, i, a ) -> execute (getCustomFood (i, def a)) (def v)
  AddCustomFood    (d, n)     -> execute_ (addCustomFood (d, n))
  DeleteCustomFood i          -> execute_ (deleteCustomFood i)

run :: IO ()
run = runReaderT (exec >>= liftIO . putStrLn) cnf
 where
  exec = executeCommand
    (ViewCustomFood (Nothing, Id 1, Nothing))
  cnf = AppConfig { host     = "localhost"
                  , port     = 8080
                  , username = "stef1"
                  , password = "password1"
                  }
