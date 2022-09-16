{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Model.DateTime
  ( Date(..)
  , Time(..)
  , Minute(..)
  , Inteval(..)
  , formatted
  , timeToMinutes
  , minutesToTime
  , TimeRound(..)
  ) where
import           Data.Aeson
import           Data.Text                      ( Text )
import           Fmt
import           GHC.Generics
import           Network.HTTP.Req
import           Typeclass.AsQueryParam
import           Typeclass.Formatted
import           Typeclass.WithDefault

-- year month day
data Date = Date
  { year  :: Integer
  , month :: Integer
  , day   :: Integer
  }

-- hour minute
data Time = Time
  { hour   :: Integer
  , minute :: Integer
  }

newtype Minute    = Minute Integer
newtype TimeRound = TimeRound Integer

data Inteval = Inteval Time Time

timeToMinutes :: Time -> Minute
timeToMinutes Time { hour, minute } = Minute $ hour * 60 + minute

minutesToTime :: Minute -> Time
minutesToTime (Minute m) = Time (m `div` 60) (m `mod` 60)

instance Formatted Date where
  formatted (Date { year, month, day }) =
    "" +| year |+ "-" +| m |+ "-" +| d |+ ""
   where
    m :: Text
    m = if month < 10 then "0" +| month |+ "" else pretty month
    d :: Text
    d = if day < 10 then "0" +| day |+ "" else pretty day

instance Formatted Time where
  formatted Time { hour, minute } = hour |+ ":" +| minute |+ ""

instance Formatted Inteval where
  formatted (Inteval b e) = "" +| formatted b |+ "-" +| formatted e |+ ""

instance AsQueryParam Inteval where
  qparam (Inteval b e) =
    let (Minute start, Minute end) = (timeToMinutes b, timeToMinutes e)
    in  "interval" =: (("" +| start |+ "-" +| end |+ "") :: String)

instance AsQueryParam Date where
  qparam d = "date" =: ((pretty $ formatted d) :: String)

instance WithDefault TimeRound where
  withDefault = TimeRound 15

instance WithDefault Inteval where
  withDefault = Inteval (minutesToTime 0) (minutesToTime 1440)

instance ToJSON Inteval where
  toJSON (Inteval b e) =
    object ["start" .= timeToMinutes b, "end" .= timeToMinutes e]
  toEncoding (Inteval b e) =
    pairs ("start" .= timeToMinutes b <> "end" .= timeToMinutes e)

deriving instance Num Minute

deriving instance Show TimeRound
deriving instance Show Inteval
deriving instance Show Minute
deriving instance Show Time
deriving instance Show Date

deriving instance Generic Minute
deriving instance Generic Time
deriving instance Generic Date

instance ToJSON Minute
instance ToJSON Date
instance ToJSON Time

instance FromJSON Minute
instance FromJSON Date
instance FromJSON Time
