{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Types
  ( Verbosity(..)
  , Amount(..)
  , Id(..)
  , Minute(..)
  , Description(..)
  , Date(..)
  , Time(..)
  , Offset(..)
  , PageLimit(..)
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )


data Verbosity = Minimal | Normal | Verbose deriving (Show, Generic)

instance ToJSON Verbosity
instance FromJSON Verbosity

newtype Amount = Amount Integer deriving (Show, Generic)

instance ToJSON Amount
instance FromJSON Amount

newtype Id a = Id Integer deriving (Show, Generic)

instance ToJSON (Id a)
instance FromJSON (Id a)

newtype Minute = Minute Integer deriving (Show, Generic)

instance ToJSON Minute
instance FromJSON Minute

newtype Description = Description String deriving (Show, Generic)

instance ToJSON Description
instance FromJSON Description

newtype Offset = Offset String deriving (Show, Generic)

instance ToJSON Offset
instance FromJSON Offset

-- year month day
data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  }
  deriving (Show, Generic)

instance ToJSON Date
instance FromJSON Date

-- hour minute
data Time = Time
  { hour   :: Int
  , minute :: Int
  }
  deriving (Show, Generic)

instance ToJSON Time
instance FromJSON Time

data PageLimit = PageLimit
  { page  :: Integer
  , limit :: Integer
  }
  deriving (Show, Generic)

instance ToJSON PageLimit
instance FromJSON PageLimit
