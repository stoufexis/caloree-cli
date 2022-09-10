module Main
  ( main
  ) where

import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , ReaderT(runReaderT)
                                                )
import           Data.String
import qualified Data.Text                     as T
import           Data.Time
import           Lib                            ( executeCommand )
import           Model.Config
import           Model.DateTime
import           Parse.ParseCommand             ( parseCommand )
import           System.Environment             ( getEnv )

getTime :: IO Time
getTime = fmap (toTime . localTimeOfDay . zonedTimeToLocalTime) getZonedTime
 where
  toTime TimeOfDay { todHour, todMin } =
    Time { hour = toInteger todHour, minute = toInteger todMin }

getDate :: IO Date
getDate = do
  now      <- getCurrentTime
  timezone <- getCurrentTimeZone
  let zoneNow            = utcToLocalTime timezone now
  let (year, month, day) = toGregorian $ localDay zoneNow
  pure $ Date year (toInteger month) (toInteger day)


main :: IO ()
main = cnf >>= runReaderT exec
 where
  exec = parseCommand >>= executeCommand >>= (liftIO . print)
  cnf =
    AppConfig
      <$> fmap T.pack     (getEnv "HOST")
      <*> fmap read       (getEnv "PORT")
      <*> fmap fromString (getEnv "USERNAME")
      <*> fmap fromString (getEnv "PASSWORD")
      <*> getDate
      <*> getTime





