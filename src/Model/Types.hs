{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Types
  ( Verbosity(..)
  , Amount(..)
  , Id(..)
  , Description(..)
  , trimmed
  , PageLimit(..)
  , EFID(..)
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

data Verbosity = Minimal | Normal | Verbose

newtype Amount      = Amount Integer
newtype Id          = Id Integer
newtype Description = Description Text

-- Either custom_food_id food_id
newtype EFID = EFID (Either Id Id) deriving (Show)

data PageLimit = PageLimit
  { page  :: Integer
  , limit :: Integer
  }

trimmed :: Verbosity -> Description -> Text
trimmed Minimal (Description d) = T.take 50 d
trimmed Normal  (Description d) = T.take 100 d
trimmed Verbose (Description d) = d

instance Formatted Amount where
  formatted (Amount a) = a |+ " gr"

instance Formatted Id where
  formatted (Id a) = build a

instance Formatted EFID where
  formatted (EFID (Left  (Id i))) = "*" +| i |+ ""
  formatted (EFID (Right (Id i))) = build i

instance AsQueryParam EFID where
  qparam (EFID (Right (Id i))) = "food_id" =: i
  qparam (EFID (Left  (Id i))) = "custom_food_id" =: i

instance AsQueryParam Description where
  qparam (Description d) = "description" =: d

instance AsQueryParam Amount where
  qparam (Amount a) = "amount" =: a

instance AsQueryParam PageLimit where
  qparam (PageLimit { page, limit }) = "page" =: page <> "limit" =: limit

instance WithDefault PageLimit where
  withDefault = PageLimit { page = 0, limit = 25 }

instance WithDefault Verbosity where
  withDefault = Normal

instance WithDefault Amount where
  withDefault = Amount 100

deriving instance Show Verbosity
deriving instance Show Amount
deriving instance Show Id
deriving instance Show Description
deriving instance Show PageLimit

deriving instance Generic Verbosity
deriving instance Generic Amount
deriving instance Generic Id
deriving instance Generic Description
deriving instance Generic PageLimit

instance ToJSON EFID where
  toJSON (EFID (Left  i)) = object ["custom_food_id" .= i]
  toJSON (EFID (Right i)) = object ["food_id" .= i]

  toEncoding (EFID (Left  i)) = pairs ("custom_food_id" .= i)
  toEncoding (EFID (Right i)) = pairs ("food_id" .= i)

instance ToJSON Verbosity
instance ToJSON Amount
instance ToJSON Id
instance ToJSON Description
instance ToJSON PageLimit

instance FromJSON EFID where
  parseJSON v = i v <|> i' v
   where
    i  = fmap (EFID . Left) . withObject "EFID" (.: "custom_food_id")
    i' = fmap (EFID . Right) . withObject "EFID" (.: "food_id")

instance FromJSON Verbosity
instance FromJSON Amount
instance FromJSON Id
instance FromJSON Description
instance FromJSON PageLimit
