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
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
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

newtype Inteval = Inteval (Maybe Minute, Maybe Group, Maybe Offset)

newtype Amount      = Amount Integer
newtype Id          = Id Integer
newtype Minute      = Minute Integer
newtype Description = Description Text
newtype Offset      = Offset Integer
newtype Group       = Group Integer

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

instance AsQueryParam Offset where
  qparam (Offset o) = "offset" =: o

instance AsQueryParam PageLimit where
  qparam (PageLimit { page, limit }) = "page" =: page <> "limit" =: limit

instance AsQueryParam Date where
  qparam (Date { year, month, day }) =
    "date" =: (("" +| year |+ "-" +| m |+ "-" +| d |+ "") :: String)
   where
    m :: Text
    m = if month < 10 then "0" +| month |+ "" else pretty month
    d :: Text
    d = if day < 10 then "0" +| day |+ "" else pretty day

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
