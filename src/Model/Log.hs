module Model.Log
  ( Log
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Model.Nutrients                ( Nutrients )
import           Model.Types                    ( Description
                                                , Id
                                                , Minute
                                                )

data Log = Log
  { id          :: Id
  , day         :: Text
  , minute      :: Minute
  , description :: Description
  , nutrients   :: Nutrients
  }
  deriving (Show, Generic)

instance ToJSON Log
instance FromJSON Log
