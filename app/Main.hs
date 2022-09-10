module Main
  ( main
  ) where

import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , ReaderT(runReaderT)
                                                )
import           Lib                            ( executeCommand )
import           Model.Config
import           Model.DateTime
import           Parse.ParseCommand             ( parseCommand )

main :: IO ()
main = runReaderT exec cnf
 where
  exec = parseCommand >>= executeCommand >>= (liftIO . print)
  cnf  = AppConfig { host     = "localhost"
                   , port     = 8080
                   , username = "stef1"
                   , password = "password1"
                   , date     = Date { year = 2022, month = 9, day = 8 }
                   , time     = Time { hour = 17, minute = 21 }
                   }
