{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Types
  ( Verbosity(..)
  , Grams(..)
  , Id(..)
  , Description(..)
  , trimmed
  , Page(..)
  , Limit(..)
  , EFID(..)
  , Kcal(..)
  , Offset(..)
  , EntryNum(..)
  , UndoTimes(..)
  ) where
import           Control.Applicative
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Fmt
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req
import           Typeclass.AsQueryParam         ( AsQueryParam(..) )
import           Typeclass.Formatted            ( Formatted(..) )
import           Typeclass.WithDefault          ( WithDefault(..) )

data Verbosity = Minimal | Normal | Verbose deriving Enum

newtype Grams       = Grams Float
newtype Kcal        = Kcal Float
newtype Id          = Id Integer
newtype Description = Description Text
newtype Page        = Page Integer
newtype Limit       = Limit Integer
newtype Offset      = Offset Integer
newtype UndoTimes   = UndoTimes Integer
newtype EntryNum    = EntryNum Integer

-- Either custom_food_id food_id
newtype EFID = EFID (Either Id Id) deriving Show

trimmed :: Verbosity -> Description -> Text
trimmed Minimal (Description d) = T.take 50 d
trimmed Normal  (Description d) = T.take 100 d
trimmed Verbose (Description d) = d

instance Formatted UndoTimes where
  formatted (UndoTimes t) = build t

instance Formatted EntryNum where
  formatted (EntryNum t) = build t

instance Formatted Description where
  formatted (Description t) = build $ show t

instance Formatted Page where
  formatted (Page a) = build a

instance Formatted Limit where
  formatted (Limit a) = build a

instance Formatted Grams where
  formatted (Grams a) = a |+ " gr"

instance Formatted Kcal where
  formatted (Kcal a) = a |+ " kcal"

instance Formatted Id where
  formatted (Id a) = build a

instance Formatted EFID where
  formatted (EFID (Left  (Id i))) = "c" <> build i
  formatted (EFID (Right (Id i))) = "f" <> build i

instance Formatted Verbosity where
  formatted = pretty . fromEnum

instance AsQueryParam Offset where
  qparam (Offset o) = "offset" =: o

instance AsQueryParam EFID where
  qparam (EFID (Right (Id i))) = "food_id" =: i
  qparam (EFID (Left  (Id i))) = "custom_food_id" =: i

instance AsQueryParam Description where
  qparam (Description d) = "description" =: d

instance AsQueryParam Grams where
  qparam (Grams a) = "grams" =: a

instance AsQueryParam Page where
  qparam (Page page) = "page" =: page

instance AsQueryParam Limit where
  qparam (Limit limit) = "limit" =: limit

instance WithDefault UndoTimes where
  withDefault = UndoTimes 0

instance WithDefault Description where
  withDefault = Description ""

instance WithDefault Page where
  withDefault = Page 0

instance WithDefault Limit where
  withDefault = Limit 25

instance WithDefault Verbosity where
  withDefault = Normal

instance WithDefault Grams where
  withDefault = Grams 100

deriving instance Num Grams
deriving instance Num Kcal

deriving instance Show UndoTimes
deriving instance Show Verbosity
deriving instance Show Kcal
deriving instance Show Grams
deriving instance Show Id
deriving instance Show Description
deriving instance Show Page
deriving instance Show Limit

deriving instance Generic UndoTimes
deriving instance Generic Verbosity
deriving instance Generic Kcal
deriving instance Generic Grams
deriving instance Generic Id
deriving instance Generic Description
deriving instance Generic Page
deriving instance Generic Limit

instance ToJSON EFID where
  toJSON (EFID (Left  i)) = object ["custom_food_id" .= i]
  toJSON (EFID (Right i)) = object ["food_id" .= i]

  toEncoding (EFID (Left  i)) = pairs ("custom_food_id" .= i)
  toEncoding (EFID (Right i)) = pairs ("food_id" .= i)

instance ToJSON UndoTimes
instance ToJSON Verbosity
instance ToJSON Kcal
instance ToJSON Grams
instance ToJSON Id
instance ToJSON Description
instance ToJSON Page
instance ToJSON Limit

instance FromJSON EFID where
  parseJSON v = i v <|> i' v
   where
    i  = fmap (EFID . Left) . withObject "EFID" (.: "custom_food_id")
    i' = fmap (EFID . Right) . withObject "EFID" (.: "food_id")

instance FromJSON Verbosity
instance FromJSON Kcal
instance FromJSON Grams
instance FromJSON Id
instance FromJSON Description
instance FromJSON Page
instance FromJSON Limit
