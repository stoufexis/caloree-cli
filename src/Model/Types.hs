{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Types
  ( Verbosity(..)
  , Amount(..)
  , Id(..)
  , Minute(..)
  , Description(..)
  , trimmed
  , Date(..)
  , Time(..)
  , Offset(..)
  , PageLimit(..)
  , Group(..)
  , Inteval(..)
  , EFID(..)
  , formatted
  , timeToMinutes
  ) where
import           Control.Applicative
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Fmt
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req
import           Typeclass.AsQueryParam         ( AsQueryParam(..) )
import           Typeclass.WithDefault          ( WithDefault(..)
                                                , def
                                                )


data Verbosity = Minimal | Normal | Verbose

newtype Inteval = Inteval (Maybe Minute, Maybe Group, Maybe Offset) deriving Show

newtype Amount      = Amount Integer
newtype Id          = Id Integer
newtype Minute      = Minute Integer
newtype Description = Description Text
newtype Offset      = Offset Integer
newtype Group       = Group Integer

-- Either custom_food_id food_id
newtype EFID = EFID (Either Id Id) deriving (Show)

-- year month day
data Date = Date
  { year  :: Integer
  , month :: Integer
  , day   :: Integer
  }

formatted :: Date -> Text
formatted (Date { year, month, day }) =
  "" +| year |+ "-" +| m |+ "-" +| d |+ ""
 where
  m :: Text
  m = if month < 10 then "0" +| month |+ "" else pretty month
  d :: Text
  d = if day < 10 then "0" +| day |+ "" else pretty day


-- hour minute
data Time = Time
  { hour   :: Integer
  , minute :: Integer
  }

timeToMinutes :: Time -> Minute
timeToMinutes Time { hour, minute } = Minute $ hour * 60 + minute

data PageLimit = PageLimit
  { page  :: Integer
  , limit :: Integer
  }

trimmed :: Verbosity -> Description -> Text
trimmed Minimal (Description d) = T.take 50 d
trimmed Normal  (Description d) = T.take 100 d
trimmed Verbose (Description d) = d

instance AsQueryParam Inteval where
  qparam (Inteval (grouping, group, offset)) =
    let (Offset o) = def offset
        (Minute m) = def grouping
        (Group  g) = def group
        start      = m * g + o
        end        = start + m
    in  "interval" =: (("" +| start |+ "-" +| end |+ "") :: String)

instance AsQueryParam EFID where
  qparam (EFID (Right (Id i))) = "food_id" =: i
  qparam (EFID (Left  (Id i))) = "custom_food_id" =: i

instance AsQueryParam Description where
  qparam (Description d) = "description" =: d

instance AsQueryParam Offset where
  qparam (Offset o) = "offset" =: o

instance AsQueryParam Amount where
  qparam (Amount a) = "amount" =: a

instance AsQueryParam PageLimit where
  qparam (PageLimit { page, limit }) = "page" =: page <> "limit" =: limit

instance AsQueryParam Date where
  qparam d = "date" =: formatted d

instance WithDefault Inteval where
  withDefault = Inteval (Just withDefault, Just withDefault, Just withDefault)

instance WithDefault Group where
  withDefault = Group 0

instance WithDefault Minute where
  withDefault = Minute 1440

instance WithDefault Offset where
  withDefault = Offset 0

instance WithDefault PageLimit where
  withDefault = PageLimit { page = 0, limit = 25 }

instance WithDefault Verbosity where
  withDefault = Normal

instance WithDefault Amount where
  withDefault = Amount 100

deriving instance Show Group
deriving instance Show Verbosity
deriving instance Show Amount
deriving instance Show Id
deriving instance Show Minute
deriving instance Show Description
deriving instance Show Offset
deriving instance Show Time
deriving instance Show Date
deriving instance Show PageLimit

deriving instance Generic Group
deriving instance Generic Verbosity
deriving instance Generic Amount
deriving instance Generic Id
deriving instance Generic Minute
deriving instance Generic Description
deriving instance Generic Offset
deriving instance Generic Time
deriving instance Generic Date
deriving instance Generic PageLimit

instance ToJSON EFID where
  toJSON (EFID (Left  i)) = object ["custom_food_id" .= i]
  toJSON (EFID (Right i)) = object ["food_id" .= i]

  toEncoding (EFID (Left  i)) = pairs ("custom_food_id" .= i)
  toEncoding (EFID (Right i)) = pairs ("food_id" .= i)

instance ToJSON Inteval where
  toJSON (Inteval (grouping, group, offset)) =
    let (Offset o) = def offset
        (Minute m) = def grouping
        (Group  g) = def group
        start      = m * g + o
        end        = start + m
    in  object ["start" .= start, "end" .= end]

  toEncoding (Inteval (grouping, group, offset)) =
    let (Offset o) = def offset
        (Minute m) = def grouping
        (Group  g) = def group
        start      = m * g + o
        end        = start + m
    in  pairs ("start" .= start <> "end" .= end)

instance ToJSON Group
instance ToJSON Verbosity
instance ToJSON Amount
instance ToJSON Id
instance ToJSON Minute
instance ToJSON Description
instance ToJSON Offset
instance ToJSON Date
instance ToJSON Time
instance ToJSON PageLimit

instance FromJSON EFID where
  parseJSON v = i v <|> i' v
   where
    i :: Value -> Parser EFID
    i = fmap (EFID . Left) . withObject "EFID" (.: "custom_food_id")

    i' :: Value -> Parser EFID
    i' = fmap (EFID . Right) . withObject "EFID" (.: "food_id")

instance FromJSON Group
instance FromJSON Verbosity
instance FromJSON Amount
instance FromJSON Id
instance FromJSON Minute
instance FromJSON Description
instance FromJSON Offset
instance FromJSON Date
instance FromJSON Time
instance FromJSON PageLimit
