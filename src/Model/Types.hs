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
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Typeclass.WithDefault          ( WithDefault(..) )


data Verbosity = Minimal | Normal | Verbose

newtype Amount      = Amount Integer
newtype Id          = Id Integer
newtype Minute      = Minute Integer
newtype Description = Description Text
newtype Offset      = Offset String

-- year month day
data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  }

-- hour minute
data Time = Time
  { hour   :: Int
  , minute :: Int
  }

data PageLimit = PageLimit
  { page  :: Integer
  , limit :: Integer
  }

trimmed :: Verbosity -> Description -> Text
trimmed Minimal (Description d) = T.take 50 d
trimmed Normal  (Description d) = T.take 100 d
trimmed Verbose (Description d) = d

instance WithDefault PageLimit where
  withDefault = PageLimit { page = 0, limit = 25 }

instance WithDefault Verbosity where
  withDefault = Normal

instance WithDefault Amount where
  withDefault = Amount 100

deriving instance Show Verbosity
deriving instance Show Amount
deriving instance Show Id
deriving instance Show Minute
deriving instance Show Description
deriving instance Show Offset
deriving instance Show Time
deriving instance Show Date
deriving instance Show PageLimit

deriving instance Generic Verbosity
deriving instance Generic Amount
deriving instance Generic Id
deriving instance Generic Minute
deriving instance Generic Description
deriving instance Generic Offset
deriving instance Generic Time
deriving instance Generic Date
deriving instance Generic PageLimit

instance ToJSON Verbosity
instance ToJSON Amount
instance ToJSON Id
instance ToJSON Minute
instance ToJSON Description
instance ToJSON Offset
instance ToJSON Date
instance ToJSON Time
instance ToJSON PageLimit

instance FromJSON Verbosity
instance FromJSON Amount
instance FromJSON Id
instance FromJSON Minute
instance FromJSON Description
instance FromJSON Offset
instance FromJSON Date
instance FromJSON Time
instance FromJSON PageLimit
