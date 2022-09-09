{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Model.DateTime
  ( Date(..)
  , Time(..)
  , Minute(..)
  , Offset(..)
  , Group(..)
  , Inteval(..)
  , formatted
  , timeToMinutes
  , toRange
  , minutesToTime
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

newtype Minute = Minute Integer
newtype Offset = Offset Integer
newtype Group  = Group Integer

newtype Inteval = Inteval (Maybe Minute, Maybe Group, Maybe Offset)

toRange :: Inteval -> (Integer, Integer)
toRange (Inteval (grouping, group, offset)) =
  let (Offset o) = def offset
      (Minute m) = def grouping
      (Group  g) = def group
      start      = m * g + o
      end        = start + m
  in  (start, end)

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
  formatted i =
    let (start, end) = toRange i in "" +| start |+ "-" +| end |+ ""

instance AsQueryParam Inteval where
  qparam i =
    let (start, end) = toRange i
    in  "interval" =: (("" +| start |+ "-" +| end |+ "") :: String)

instance AsQueryParam Offset where
  qparam (Offset o) = "offset" =: o

instance AsQueryParam Date where
  qparam d = "date" =: ((pretty $ formatted d) :: String)

instance WithDefault Inteval where
  withDefault = Inteval (Just withDefault, Just withDefault, Just withDefault)

instance WithDefault Group where
  withDefault = Group 0

instance WithDefault Minute where
  withDefault = Minute 1440

instance WithDefault Offset where
  withDefault = Offset 0

instance ToJSON Inteval where
  toJSON i =
    let (start, end) = toRange i in object ["start" .= start, "end" .= end]

  toEncoding i =
    let (start, end) = toRange i in pairs ("start" .= start <> "end" .= end)

deriving instance Show Inteval
deriving instance Show Group
deriving instance Show Minute
deriving instance Show Offset
deriving instance Show Time
deriving instance Show Date

deriving instance Generic Group
deriving instance Generic Minute
deriving instance Generic Offset
deriving instance Generic Time
deriving instance Generic Date

instance ToJSON Group
instance ToJSON Minute
instance ToJSON Offset
instance ToJSON Date
instance ToJSON Time

instance FromJSON Group
instance FromJSON Minute
instance FromJSON Offset
instance FromJSON Date
instance FromJSON Time
